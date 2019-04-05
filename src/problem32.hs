import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Vector.Unboxed as VU

main = print $ sum $ nub $ concat $ filter
  (/= [])
  [ fmap readInt $ catMaybes [check 1 5 v, check 2 5 v, check 2 4 v]
  | p <- permutations "123456789"
  , let v = VU.fromList $ map digitToInt p
  ]
 where
  -- [start, end)
  slice :: VU.Unbox a => Int -> Int -> VU.Vector a -> VU.Vector a
  slice s e = VU.slice s (e - s)

  readInt :: VU.Vector Int -> Int
  readInt = VU.foldl' (\acc ch -> acc * 10 + ch) 0

  check :: Int -> Int -> VU.Vector Int -> Maybe (VU.Vector Int)
  check p q s
    | readInt (slice 0 p s) * readInt (slice p q s) == readInt (slice q 9 s)
    = Just (slice q 9 s)
    | otherwise
    = Nothing

