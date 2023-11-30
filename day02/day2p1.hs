#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

import Data.List.Split

main :: IO ()
main = interact $ show . runOpCode 0 . twelveOTwo . map read . splitWhen (==',')

twelveOTwo :: [Int] -> [Int]
twelveOTwo = update 1 12 . update 2 2

runOpCode :: Int -> [Int] -> Int
runOpCode i ls =
    case ls !! i of
        1 -> let a = ls !! (ls !! (i+1))
                 b = ls !! (ls !! (i+2))
                 target = ls !! (i+3)
                 next = i + 4
             in runOpCode next (update target (a + b) ls)
        2 -> let a = ls !! (ls !! (i+1))
                 b = ls !! (ls !! (i+2))
                 target = ls !! (i+3)
                 next = i + 4
             in runOpCode next (update target (a * b) ls)
        99 -> ls !! 0
        _ -> error "Ended up at non-opcode"

update :: Int -> Int -> [Int] -> [Int]
update t v ls = take t ls ++ [v] ++ drop (t+1) ls
