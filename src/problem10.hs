import Control.Monad
import Data.IORef
import qualified Data.Vector.Unboxed.Mutable as V

bound = 2000000

primeArray :: IO (V.IOVector Bool)
primeArray = do
  vec <- V.replicate bound False
  forM_ (takeWhile (\n -> n * n <= bound) [2..bound]) $ \n -> do
    forM_ (takeWhile (< bound) $ map (*n) $ tail [1..]) $ \i -> V.write vec i True

  return vec

solve :: IO Int
solve = do
  sum <- newIORef 0
  vec <- primeArray
  forM_ [2..bound-1] $ \i -> do
    r <- V.read vec i
    when (not r) $ modifyIORef sum (+i)

  readIORef sum

---

primes = 2 : 3 : [n | n <- [4..], all (\p -> n `mod` p /= 0) $ takeWhile (\p -> p * p <= n) primes]

solve' = sum $ takeWhile (< bound) primes

main = print $ solve'

