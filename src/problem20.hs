import Data.List

bound = 100

main = print $ sum $ map (read . return) $ show $ foldl' go 1 [1..bound] where
  go a b
    | a `mod` 10 == 0 = go (a `div` 10) b
    | b `mod` 10 == 0 = go a (b `div` 10)
    | otherwise = a * b


