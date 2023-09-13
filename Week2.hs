

maxhr :: Float -> Float
maxhr age = 
    if age > 40
    then 207.0 - 0.7 * age
    else 220.0 - age




points :: Int -> [(Int, Int)]
points x = zip [0..x] [x,x-1..0]