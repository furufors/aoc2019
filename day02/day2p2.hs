#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

import Data.List.Split

main :: IO ()
main = interact $ show . run . map read . splitWhen (==',')

run :: [Int] -> Int
run pg = head [100 * a + b | a <- [0..99], b <- [0..99], runOpCode 0 (s a b) == 19690720]
    where
        s noun verb = update 1 noun . update 2 verb $ pg

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
