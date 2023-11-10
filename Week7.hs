
-- :load 
-- thank yourself later

--HW 02

distanceFilter :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
distanceFilter f d s ss = dfHelper f d s ss []
 where
 dfHelper f d s ss result
  |ss == [] = result
 dfHelper f d s (x:ss) result
  |(f s x) <= d = dfHelper f d s ss (x:result)
  |otherwise = dfHelper f d s ss result


distance3 :: String -> String -> Float
distance3 x y = fromIntegral $ abs $ length x - length y

s = "Stop"
ss = ["Curabitur"," suscipit"," quis"," urna"," dignissim"," rutrum"]

--test :: (Int -> Int) -> Int -> [Int] -> [Int]
test f d a = filter (d >= ) $ map f a




-- HW 01

validate :: String -> Bool
validate (x:y:z)
 |x /= 'F' || y /= 'I' = False
validate x
 |length x /= 18 = False
 |valid x == 1 = True
 |otherwise = False
 where
  valid :: String -> Integer
  valid x = mod (lime $ map numerate $ ibanswitch x) 97



-- line Integers
line :: [Int] -> Int
line [] = 0
line [x] = x
line (x:y:z)
 |y < 10 = line (x*10+y:z)
 |otherwise = line (x*100+y:z)
 
lime [] = 0
lime (x) = linehelper x 0
 where
 linehelper :: [Int] -> Integer -> Integer
 linehelper (x:y) result
  |y == [] = (result*10+(toInteger x))
  |x < 10 = linehelper (y) (10*result + (toInteger x))
  |otherwise = linehelper (y) (100*result + (toInteger x))
 

-- Debug fn
numerate x = tupleswitch $ unlist $ (filter (includes x) $ alphanums)

-- Tuple to Int
tupleswitch :: (Int, Char) -> Int
tupleswitch (x,y) = x

-- unlists lists
--unlist :: [(Int, Char)] -> (Int, Char)
unlist [x] = x 

-- filter tuples
fples :: Char -> [(Int, Char)]
fples x = filter (includes x) $ alphanums

-- filter rule
includes :: Char -> (Int, Char) -> Bool
includes x (_,z)
 |x == z = True
 |otherwise = False

-- Order characters
ibanswitch :: String -> String
ibanswitch (x:y:z:w:rst) = rst ++ [x] ++ [y] ++ [z] ++ [w]



alpha = ['A'..'Z']
nums = ['0'..'9']
ibantest = "FI1111222233334444"
alphanums = zip [0,1..] $ ['0'..'9'] ++ ['A'..'Z']
inp = "FI2112345600000785"
int = "FI1010101010101010"

