{-# LANGUAGE RecordWildCards #-}
import Data.Ratio

-- a + b sqrt(base)
data SqR = SqR {
  base :: Int,
  a :: Ratio Int,
  b :: Ratio Int
} deriving (Eq, Show)

main = print $ length $ filter odd $ map
  blockOf
  [ n | n <- [1 .. 10000], floor (sqrt $ fromIntegral n) ^ 2 /= n ]
 where
  blockOf n =
    let (_, r) = split $ SqR n (fromIntegral 0) (fromIntegral 1) in block r

  block r = go' 1 $ snd $ split r
   where
    go' i r' | r == r'   = i
             | otherwise = go' (i + 1) $ snd $ split r'

  split :: SqR -> (Int, SqR)
  split (SqR {..}) = (,) n
    $ SqR {base = base, a = (fromIntegral n - a) * d, b = b * d}
   where
    n :: Int
    n =
      floor
        $ fromIntegral (numerator a)
        / fromIntegral (denominator a)
        + ( sqrt (fromIntegral base) * fromIntegral (numerator b) / fromIntegral
            (denominator b)
          )

    d :: Ratio Int
    d =
      let d' = (b ^ 2 * fromIntegral base - (fromIntegral n - a) ^ 2)
      in  denominator d' % numerator d'


