module Numeric where

bit :: Integral a => Bool -> a
bit True  = 1
bit False = 0

toi :: Integral a => a -> Integer
toi = toInteger

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

divides :: Integral a => a -> a -> Bool
divides d x = x `mod` d == 0

timesDivisibleBy :: Integral a => a -> a -> Int
timesDivisibleBy d = length . takeWhile (\r -> r `mod` d == 0) . iterate (`div` d)

(x, y) .- z = (x - z, y)
(x, y) -. z = (x, y - z)
(x, y) .+ z = (x + z, y)
(x, y) +. z = (x, y + z)
(x, y) .* z = (x * z, y)
(x, y) *. z = (x, y * z)
(x, y) ./ z = (x / z, y)
(x, y) /. z = (x, y / z)

is0 :: (Num a, Eq a) => a -> Bool
is0 = (==) 0
