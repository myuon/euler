import qualified Data.Vector.Unboxed as V

bound = 2000000
primes = 2 : 3 : [n | i <- [1..], j <- [1,-1], let n = 6*i+j, all (\p -> n `mod` p /= 0) $ takeWhile (\p -> p * p <= n) primes]

-- 15.2s
main = print $ sum $ takeWhile (<= bound) primes

