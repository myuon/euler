import Data.List

iteration = 1000

main = print $ sum $ map (read . return) $ show $ go 2 1 where
  go n k
    | k * 2 <= iteration = go (n^2) (2*k)
    | otherwise = n * 2^(iteration - k)

