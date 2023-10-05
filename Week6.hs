
-- :load 
-- thank yourself later



gap :: (Char, Char) -> Int -> String -> Int
gap (a,b) g s = gapHelper (a,b) g s 0
 where
 gapHelper _ _ [] result = result
 gapHelper (a,b) g (fst:rst) result
  |g < 0 || g >= (length rst) = result
  |fst == a && rst !! g == b = gapHelper (a,b) g rst (result + 1)
  |otherwise = gapHelper (a,b) g rst result



distance1 :: String -> String -> Float
distance1 [] [] = 0
distance1  sx sy = ((dist sx sy 0) + (dist sy sx 0))/fromIntegral ((length sx) + (length sy))
 where
 dist [] _ result = result
 dist (x:xr) (y:yr) result
  |elem x (y:yr) = dist xr (y:yr) result
  |otherwise = dist xr (y:yr) (result + 1)



distance2 :: String -> String -> Float
distance2 [] [] = 0
distance2 sx sy = ((dis sx 0) + (dis sy 0))/fromIntegral ((length sx) + (length sy))
 where 
 dis [] result = result
 dis (x:xr) result
  |elem x nums = dis xr result
  |otherwise = dis xr (result + 1)



dist :: String -> String -> Int -> Int
dist [] _ result = result
dist (x:xr) (y:yr) result
 |elem x (y:yr) = dist xr (y:yr) result
 |otherwise = dist xr (y:yr) (result + 1)


dis :: String -> Int -> Int
dis [] result = result
dis (x:xr) result
 |elem x nums = dis xr result
 |otherwise = dis xr (result + 1)
  
nums = ['0'..'9']

mystring = "aaabbb"
s = "aaabc"
ss = "aabdd"
x = "xy765"
xx = "abc2311"
