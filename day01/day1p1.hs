#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
main :: IO ()
main = interact $ show . sum . map ((\i -> (i `div` 3) - 2) . (read :: String -> Int)) . lines