--{-# LANGUAGE DataKinds #-}
--import Data.Char
--import Data.List

credits :: (Char, Int) -> (Char, Int) -> (Int)
credits ('s', 14) _ = 14
credits _ ('s', 14) = 14
credits (char1, int1) (char2, int2)
 | char1 == char2 =
 case () of
 ()|diff == 1 -> 8
   |diff == (-1) -> 8
   |diff == 0 -> 6
   |otherwise -> 2
 |diff == 0 = 6
 |diff == 1 = 4
 |diff == (-1) = 4
 |otherwise = 0
   where 
    diff = (int1 - int2)    

chr :: (Int) -> (Char)
chr 1 = 'a'
chr 2 = 'b'
chr 3 = 'c'
chr 4 = 'd'
chr 5 = 'e'
chr 6 = 'f'
chr 7 = 'g'
chr 8 = 'h'
chr 9 = 'i'
chr 10 = 'j'
chr 11 = 'k'
chr 12 = 'l'
chr 13 = 'm'
chr 14 = 'n'
chr 15 = 'o'
chr 16 = 'p'
chr 17 = 'q'
chr 18 = 'r'
chr 19 = 's'
chr 20 = 't'
chr 21 = 'u'
chr 22 = 'v'
chr 23 = 'w'
chr 24 = 'x'
chr 25 = 'y'
chr 26 = 'z'

anum = [1..26];
abet = ['a'..'z'];
azip = zip anum abet;

sort :: [Int] -> [Int]
sort input =
 [ output | output <- anum , elem output (input) ]


charsDivisibleBy :: Int -> [Char]
charsDivisibleBy 0 = []
charsDivisibleBy num =
   [chr (out) | out <- anum, out `mod` num == 0 ]
 --[out | out <- anum, num `mod` out == 0 ]

charsProductOf :: [Int] -> [Char]
charsProductOf num =
   [ chr (x*y) | x <- sort num, y <- sort num, y > x, x*y < 27]
   --[ chr (x*y) | x <- num, y <- num, y > x]
   