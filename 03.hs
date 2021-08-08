import Common
import Data.List
import Data.Maybe
import Debug.Trace

main = do
    content <- readFile "03.input"
    let addr = parse content
    putStrLn $ show $ solve1 addr
    putStrLn $ show $ solve2 addr

parse = readInt . head . lines

solve1 addr = d1 + d2
    where
        cs = courners
        --  current row index
        i = fromJust $ findIndex (> addr) cs
        --  lower bound of the row
        lo = cs !! (i - 1)
        --  upper bound of the row
        hi = cs !! i
        --  medium point at the row
        m = (lo + hi) `div` 2
        --  closest distance from row to center
        d1 = (i + 2) `div` 4
        --  distance from medium point in the row to target point
        d2 = abs $ addr - m


solve2 addr = find (> addr) l
--    where
l = 1 : 1 : next 3 (tail courners) l (0 : l)
next i (lo : hi : cs) (p0 : p1 : ps) ~(pp0 : pp1 : pp2 : pps)
    | trace ("\nnext " ++ show i ++ " " ++ show lo ++ "-" ++ show hi ++ " (" ++ show psum0 ++ "," ++ show p1 ++ ") (" ++ show ppsum0 ++ "," ++ show ppsum1 ++ "," ++ show ppsum2 ++ ") = " ++ show (psum + ppsum)) False = undefined
    | otherwise   = (psum + ppsum) : next i' cs' ps' pps'
    where
        i' = i + 1
        cs' = if (i == hi) then (hi : cs) else (lo : hi : cs)
        ps' = p1 : ps
        pps' = if i < 8 || (i >= hi - 1) then pp0 : pp1 : pp2 : pps else pp1 : pp2 : pps
        --pps' = pp0 : pp1 : pp2 : pps
        psum = psum0 + p1
        psum0 = if i == lo + 1 then p0 else 0
        ppsum = case () of _
                            | i <= 3 -> 0
                            | i <= 7 -> pp1
                            | otherwise -> ppsum0 + ppsum1 + ppsum2
        --  like 5 for 15
        ppsum2 = if (i < hi - 1) then pp2 else 0
        --  like 4 for 15
        ppsum1 = if (i >= 8) then pp1 else 1
        --  like 3 for 15
        ppsum0 = if (i > lo + 1) && (i < hi) then pp0 else 0

--  values at courners: 1, 2, 3, 5, 7, 10 ...
--  17  16  15  14  13
--  18   5   4   3  12
--  19   6   1   2  11
--  20   7   8   9  10
--  21  22  23---> ...
courners = scanl (+) 1 $ map (`div` 2) [2..]

--  147  142  133  122   59
--  304    5    4    2   57
--  330   10    1    1   54
--  351   11   23   25   26
--  362  747  806--->   ...
