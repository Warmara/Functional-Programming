

maxhr :: Float -> Float
maxhr age = 
    if age > 40
    then 207.0 - 0.7 * age
    else 220.0 - age




points :: Int -> [(Int, Int)]
points x = (zip [0..x-1] [x,x-1..0]) ++ (zip [x,x-1..1] [0,(-1)..(-x)]) ++ (zip [0,(-1)..(-x)+1] [(-x)..(-1)]) ++ (zip [(-x)..0] [0..x])