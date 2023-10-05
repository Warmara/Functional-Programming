

maxhr :: Float -> Float
maxhr age = 
    if age > 40
    then 207.0 - 0.7 * age
    else 220.0 - age




points :: Int -> [(Int, Int)]
points x = [(-xx,(-yy)) | xx <- [1..x], yy <- [(xx-x)..(x-xx)]]++[(xx,(-yy)) | xx <- [0..x], yy <- [(xx-x)..(x-xx)]]




headOrLast :: [String] -> Char -> [String]
headOrLast str char = if null str
    then []
    else      [ x | x <- str, (last x == char) ]
            ++[ x | x <- str, (head x == char) ]
