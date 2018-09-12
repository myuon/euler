
fib = 1 : 2 : zipWith (+) fib (tail fib)

main = print $ sum $ takeWhile (<= 4000000) $ filter even fib

