module Misc.ListRelated where

-- ============================================================================
-- Gets 5 occurances of chars in "abcde" from the stdin text, [abcde]{5}
-- As you enter the text, as soon as the 5th char is found it stops
-- and returns the list of chars in order of occurance.
getList = find 5 where
    find 0 = return []
    find n = do
        ch <- getChar
        if ch `elem` ['a'..'e'] then do
            tl <- find (n-1)
            return (ch : tl) else
          find n
{-
getList
> qwerty
    ^1
> ppppprrrts
> trubrrafghess
     ^2 ^3  ^4
> scar
    ^5
returns "ebaea"

getList
> aaaaa
  ^^^^^
returns "aaaaa"
-}
-- ============================================================================
