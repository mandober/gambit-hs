module Recreate.Numeric where

grComDiv :: (Integral a) => a -> a -> a
grComDiv 0 0 = error "[grComDiv] 0 0 is undefined"
grComDiv x y = grComDiv' (abs x) (abs y) where
    grComDiv' x 0 = x
    grComDiv' x y = grComDiv' y (x `rem` y)

grComDiv2 :: (Integral a) => a -> a -> a
grComDiv2 0 0 = error "0 0 is undefined"
grComDiv2 x 0 = abs x
grComDiv2 x y = grComDiv2 (abs y) ((abs x) `rem` (abs y))

-- lcm :: (Integral a) => a -> a -> a
-- lcm _ 0 = 0
-- lcm 0 _ = 0
-- lcm x y = abs ((x `quot` (grComDiv x y)) ⋆ y)
