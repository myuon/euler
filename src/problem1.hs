
bound = 1000

main = print $ sum [n | n <- [1..bound-1], n `mod` 3 == 0 || n `mod` 5 == 0]

