import Data.List
import Data.Maybe

import Common

main = do
    content <- readFile "13.input"
    let ranges = parse content
    --putStrLn $ show $ ranges
    putStrLn $ show $ solve1 ranges
    putStrLn $ show $ solve2 ranges

parse content = map (head . parseLine) $ lines content
    where
        parseLine s = [(depth, range) |
            (depth, s1) <- readsInt s,
            (":", s2) <- lex s1,
            (range, "") <- readsInt s2]


caught ranges delay = filter caught' ranges
    where
        caught' (depth, range) = pos range (depth + delay) == 0
        pos range t = min t' (n - t')
            where
                n = (range - 1) * 2
                t' = t `mod` n

        
solve1 ranges = sum $ map (uncurry (*)) $ caught ranges 0

solve2 ranges = fromJust $ findIndex (== []) $ map (caught ranges) [0..]
