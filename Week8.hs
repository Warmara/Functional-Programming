
-- :load 
-- thank yourself later


-- Common

-- main fn

commonSubstring :: String -> String -> String
commonSubstring _ "" = ""
commonSubstring "" _ = ""
commonSubstring sa sb = cSHelper sa sb sb ""


-- recursive Helper
cSHelper :: String -> String -> String -> String -> String
cSHelper [] _ _ result = result
cSHelper (a:sa) [] temp result = cSHelper sa temp temp result
cSHelper (a:sa) (b:sb) temp result
 |a==b = cSHelper sa sb sb (result ++ [a])
 |otherwise = cSHelper (a:sa) (sb) temp result




-- Clusters


-- takes list of strings and creates own cluster to each
-- -> map

clusters :: (String -> String -> Float) -> Float -> [String] -> [[String]]
clusters f d [""] = [[""]]
clusters f d ss = clusterHelper f d ss ss [[]]


-- recursive Helper

clusterHelper :: (String -> String -> Float) -> Float -> [String] -> [String] -> [[String]] -> [[String]]
clusterHelper f d (s:sw) ss result
   |(sw) == [] = (result ++ [(clusterOne f d s ss)])
   |result == [[]] = clusterHelper f d sw ss ([(clusterOne f d s ss)])
   |otherwise = clusterHelper f d sw ss (result ++ [(clusterOne f d s ss)])


-- takes string and create its cluster
-- -> Filter

clusterOne :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
clusterOne f d s ss = filter (condition f d s) ss


-- condition for evaluating cluster
-- -> Bool

condition :: (String -> String -> Float) -> Float -> String -> String -> Bool
condition f d s ss
 |f s ss <= d = True
 |otherwise = False

d :: Float
d=0.3
ss=["aaabc", "aabdd", "a", "aa", "abdd", "bcbcb", "", "abcdefghij"]








--- Week 6:

gap :: (Char, Char) -> Int -> String -> Int
gap (a,b) g s = gapHelper (a,b) g s 0
 where
 gapHelper _ _ [] result = result
 gapHelper (a,b) g (fst:rst) result
  |g < 0 || g >= (length rst) = result
  |fst == a && rst !! g == b = gapHelper (a,b) g rst (result + 1)
  |otherwise = gapHelper (a,b) g rst result



distance1 :: String -> String -> Float
distance1 _ [] = 0
distance1 [] _ = 0
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
