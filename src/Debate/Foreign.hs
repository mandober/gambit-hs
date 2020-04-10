module Debate.Foreign where

import Foreign.Storable

-- print (sizeOf (undefined :: Int))
sizeOfInt = sizeOf (undefined :: Int)  -- 8
