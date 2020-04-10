module Debate.Recursion where

-- unary

fac n | n < 0  = 0
      | n == 0 = 1
      | n > 0  = n * fac (n - 1)

sqs n | n < 0  = 0
      | n == 0 = 1
      | n > 0  = n*n + sqs (n - 1)

fib n | n < 0  = 0
      | n == 0 = 0
      | n == 1 = 1
      | n > 1  = fib (n-1) + fib (n-2)

len [] = 0
len (x:xs) = 1 + len xs

-- binary

add 0 m = m
add n 0 = n
add n m = succ $ add m (n-1)


{-
ABSTRACTING RECURSION
=====================



`inductive` function has to receive:
* n
* base_pairs:
  - list of basepairs: [ (cond1, ret1), (cond2, ret2), ..., (cond_n, ret_n) ]
    a pair is: (a condition function, the value to return if cond holds)
* rec_pair:
  - a pair of cond and comb functions: (rcond, comb)

n = 5
base_pairs = [ ((<0), 0), ((==0), 1), ((==1), 1) ]
rec_pair = ( (>1), comb)
comb = (+)

- id = 0
- base (==0)  [baseConds]
- recc (>0)   [recCond]

conditions:
cs = [(>1), (==0)]
cs :: (Ord a, Num a) => [a -> Bool]
-}
-- induction n basecond reccond id recomb
-- induction n id recomb

induction n id recomb
    | n == id = 1
    | n >  id = recomb n (induction (n-1) id recomb)


c1 = induction 5 0 (+) -- triangular
c2 = induction 5 0 (*) -- factorial

-- squares sum: n*n + recur (n - 1)
sumSqr n = induction n 0 (\x y -> x*x + y)
c3 = sumSqr 10

-- fib (n-2) + fib (n-1)
-- fibi n = induction n 0 (\x y -> (x (n-2)) + y)
f n = induction n 0 (\x y -> (2^x + y))


-- ============================================================================
{-

[] ++ x = x                                      (a.1)
(x :  ys) ++ z = x :  (y ++ z )                  (a.2)
(x ++ ys) ++ z = x ++ (y ++ z )                  (a.3)

The transformation eliminates append from expressions of the form:
`(f x1...xn) ++ y` by defining a function f' such that:

f' x1...xn ys = (f x1...xn) ++ ys             (a.4)

reverse' xs ys = (reverse xs) ++ ys

reverse' [] ys = ys
reverse' (x:xs) ys = reverse' xs (x:ys)

-}

-- Replacing calls of the form `reverse xs` by `reverse' xs []` leads to a
-- dramatic improvement from O(n2) to O(n) in both time and space!
rev lst = reverse' lst [] where
    reverse' [] ys = ys
    reverse' (x:xs) ys = reverse' xs (x:ys)

r1 = rev [1,2,3,4]
