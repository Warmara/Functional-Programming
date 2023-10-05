isDigit :: Char -> Bool
isDigit '1' = True
isDigit '2' = True
isDigit '3' = True
isDigit '4' = True
isDigit '5' = True
isDigit '6' = True
isDigit '7' = True
isDigit '8' = True
isDigit '9' = True
isDigit '0' = True
isDigit _ = False


nextIsGreater :: [Int] -> [Int]
nextIsGreater lst = subGreater lst ([])
 where 
 subGreater [] result = result
 subGreater [x] result = result
 subGreater (fst:(snd:rst)) (result)
  |diff > 0 = subGreater (snd:rst) (result ++ [fst]) 
  |otherwise = subGreater (snd:rst) (result)
   where
   diff = (snd - fst)



onlyDigits :: String -> Bool
onlyDigits [] = False
onlyDigits lst = subDigits lst True
 where
 subDigits [] result = result
 subDigits _ False = False
 subDigits (fst:rst) True
  |(isDigit fst) = subDigits (rst) True
  |otherwise = False