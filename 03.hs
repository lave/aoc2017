import Common
import Data.List
import Data.Maybe
import Debug.Trace

main = do
    line <- readSingleLineFile "03.input"
    let addr = readInt line
    putStrLn $ show $ solve1 addr
    putStrLn $ show $ solve2 addr


--  values at corners: 1, 2, 3, 5, 7, 10 ...
--  17  16  15  14  13
--  18   5   4   3  12
--  19   6   1   2  11
--  20   7   8   9  10
--  21  22  23---> ...
corners = scanl (+) 1 $ map (`div` 2) [2..]

solve1 addr = d1 + d2
    where
        cs = corners
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


--  147  142  133  122   59
--  304    5    4    2   57
--  330   10    1    1   54
--  351   11   23   25   26
--  362  747  806--->   ...
--
--  how previous values are defined when current value goes around the corner
--   .   .   c   p1  p0      .   c   p1  p0  x       c   p1  p0  x   x       p1  p0  x   x   x       p0  x   x   x   x
--   .  pp2 pp1 pp0  x  -->  .  pp1 pp0  x   x  -->  .  pp1 pp0  x   x  -->  c  pp1 pp0  x   x  -->  p1 pp0  x   x   x
--   .   x   x   x   x       .  pp2  x   x   x       .  pp2  x   x   x       .  pp2  x   x   x       c  pp1  x   x   x     
--   .   x   x   x   x       .   x   x   x   x       .   x   x   x   x       .   x   x   x   x       .  pp2  x   x   x
solve2 addr = fromJust $ find (> addr) l
    where
        --  super-cool technique of calculating infinite list using previous values from this previous list
        l = 1 : 1 : next 3 (tail corners) l (0 : l)
        next i cs@(lo : cs_@(hi : _)) (p0 : ps_@(p1 : _)) ~pps@(pp0 : pps_@(pp1 : pp2 : _))
            -- | trace ("\nnext " ++ show i ++ " " ++ show lo ++ "-" ++ show hi ++ " (" ++ show p0' ++ "," ++ show p1 ++ ") (" ++ show pp0' ++ "," ++ show pp1' ++ "," ++ show pp2' ++ ") = " ++ show sum) False = undefined
            | otherwise   = sum : next i' cs' ps' pps'
            where
                i' = i + 1
                --  pop corner when we reach it
                cs' = if i == hi then cs_ else cs
                --  always advance previous values for current row
                ps' = ps_
                --  advance previous values for previous row when we're not before or at the corner (we want to keep pp1
                --  at it's current position - see schema above)
                pps' = if i < hi - 1 then pps_ else pps
                --  last element from current row - only count it when right past the corner
                p0' = if i == lo + 1 then p0 else 0
                --  previous element from current row - always count it
                p1' = p1
                --  last element from previous row - don't count it when in corner or right past the corner
                pp0' = if (i > lo + 1) && (i < hi) then pp0 else 0
                --  middle element from previous row - always count it starting from 3rd element
                pp1' = if i > 3 then pp1 else 0
                --  first element from previous row - don't count it when right before or at the corner
                pp2' = if i < hi - 1 then pp2 else 0
                -- sum of all elements from current and previous rows
                sum = p0' + p1' + pp0' + pp1' + pp2'
