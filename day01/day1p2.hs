#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
main :: IO ()
main = interact $ show . sum . map (calculateFuel . (read :: String -> Int)) . lines

calculateFuel :: Int -> Int
calculateFuel i | i > 0 = let addedFuel = (i `div` 3) - 2 in if addedFuel > 0 then addedFuel + calculateFuel addedFuel else 0
calculateFuel _ = 0