module Debate.Monads1 where
{-
Haskell Programming From First Principles
18. Monad



For our purposes, (*>) and (>>) are the same thing:
    (*>) :: Applicative f => f a -> f b -> f b
    (>>) :: Monad f       => f a -> f b -> f b

sequencing fns, but with two different constraints. They should in all cases do the same thing:

    λ> putStrLn "Hello, " >> putStrLn "World!"
    λ> putStrLn "Hello, " *> putStrLn "World!"

We can see what `do` syntax looks like after the compiler desugars it
for us by manually transforming it ourselves:
-}
import Control.Applicative ((*>))

sequencing :: IO ()
sequencing = do
    putStrLn "blah"
    putStrLn "another thing"

sequencing' :: IO ()
sequencing' =
    putStrLn "blah" >>
    putStrLn "another thing"

sequencing'' :: IO ()
sequencing'' =
    putStrLn "blah" *>
    putStrLn "another thing"

-- We can do the same with the variable binding that `do` syntax includes

binding :: IO ()
binding = do
    name <- getLine
    putStrLn name

binding' :: IO ()
binding' = getLine >>= putStrLn

{-
Instead of naming the variable and passing it as arg to the
next function, we just use >>= which passes it directly

When fmap alone isn't enough
============================
if you try to fmap putStrLn over getLine, it won't do anything.

    λ> putStrLn <$> getLine

Whatever the input it won't print it, although it seems like it should due to the putStrLn being mapped over the getLine. It evaluated the IO action that requests input, but not the one that prints it...Why? Let's check the type:

    λ> :t putStrLn <$> getLine
    putStrLn <$> getLine :: IO (IO ())

First, getLine performs IO to get a String
    getLine :: IO String

putStrLn takes a String, performs IO, and returns nothing interesting:
    putStrLn :: String -> IO ()

The type of fmap as it concerns putStrLn and getLine:
The type we start with:
    <$> :: Functor f => (a -> b) -> f a -> f b

The (a -> b) arg corresponds to putStrLn:
    --          (a     -> b   )
    putStrLn :: String -> IO ()

So `a` is String and `b` is `IO ()`.
The `f a` arg corresponds to getLine:
               f  a
    getLine :: IO String

So `f` is IO.
The output `f b` corresponds to mapping putStrLn over getLine:
    (a     -> b   )      f  a         f   b
    String -> IO ()  <$> IO String :: f   IO ()
    putStrLn         <$> getLine   :: IO (IO ())

a :: String
b :: IO ()
(a -> b) :: String -> IO ()

  a ::    String
f a :: IO String
f   :: IO

  b ::     IO ()
f b :: IO (IO ())


The `b` gets specialized to the type `IO ()`, which is going to jam another IO action inside of the IO that getLine performs.

So this is what is happening with our types:

    f :: Functor f => f String -> f (IO ())
    f x = putStrLn <$> x

    g :: (String -> b) -> IO b
    g x = x <$> getLine

    putStrLn <$> getLine :: IO (IO ())
                            1   2  3

3: is the unit that putStrLn returns.
2: inner IO repr the action that would be performed if putStrLn was evaluated.
1: outer IO repr the action getLine must perform to get the user input String.

Composing actions
=================
We can refer to, compose, and map over actions without performing them.
We can wait to evaluate IO actions or any computation in general.

    λ> printOne = putStrLn "1"
    λ> printTwo = putStrLn "2"
    λ> twoActions = (printOne, printTwo)
    λ> :t twoActions
    twoActions :: (IO (), IO ())

Now that we have a pair of two IO actions defined, we can grab one to eval it;
we can evaluate an IO action multiple times.

    λ> fst twoActions
    1
    λ> snd twoActions
    2
    λ> fst twoActions
    1

So, when we fmap putStrLn over getLine we need to join those two IO layers together and for that we need the unique thing that Monad offers, `join`:

    λ> import Control.Monad (join)
    λ> join $ putStrLn <$> getLine
    qwerty
    qwerty
    λ> :t join $ putStrLn <$> getLine
    join $ putStrLn <$> getLine :: IO ()

What `join` did here is merge the effects of getLine and putStrLn into a single IO action. This merged IO action performs the effects in the ORDER determined by the nesting of the IO actions. As it happens, the cleanest way to express ORDERING in a lambda calculus is through nesting of expressions or lambdas.

So we're still dealing with lambda calculus; monadic sequencing and `do` syntax seem very far removed from monads but they are not. Monadic actions are still pure, and the sequencing operations are just ways of nesting lambdas.

Now, IO is different as it allows side effects, but since those effects are constrained in IO type, the rest of it is still pure lambda calculus.

It could be valuable having ability to suspend or cancell an IO action until some determination is made, so types like `IO (IO ())` are not invalid.

We get back to desugaring `do` syntax with the new understanding of what monads do for us:

bindingAndSequencing :: IO ()
bindingAndSequencing = do
    putStrLn "name pls:"
    name <- getLine
    putStrLn ("hi: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
    putStrLn "name pls:" >>
    getLine >>=
    \name -> putStrLn ("hi: " ++ name)


As the nesting intensifies, you can see how `do` syntax can make things
a bit cleaner and easier to read:

                    twoBinds :: IO ()
twoBinds = do                       twoBinds' =
    putStrLn "name pls:"                putStrLn "name pls:" >>
    name <- getLine                     getLine >>=
    putStrLn "age pls:"                 \name -> putStrLn "age pls:" >>
    age <- getLine                      getLine >>=
    putStrLn ("hi: "                    \age -> putStrLn ("hi: "
                    ++ name ++ " who is: "
                    ++ age ++ " years old.")


Here is the same example as above, only this time with parens:


twoBinds' :: IO ()
twoBinds' = putStrLn "name pls:" >> getLine >>=
    (\name -> putStrLn "age pls: " >> getLine >>=
        (\age -> putStrLn $ "hi" ++ name ++ "age" ++ age ++ "old")
    )

-}
