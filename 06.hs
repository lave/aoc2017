import Common
import Data.Array
import Data.Foldable
import Data.List
import Data.Maybe
import Debug.Trace

main = do
    line <- readSingleLineFile "06.input"
    let mem = parse line
    -- putStrLn $ show $ mem
    let mems = iterate realloc mem
    let Just (i, mem') = firstDup mems
    putStrLn $ show i
    let Just i0 = elemIndex mem' mems
    putStrLn $ show $ i - i0

parse s = listArray (0, length l - 1) l
    where
        l = map readInt $ words s


realloc mem
    -- | trace ("\nw = " ++ show w ++ ", m = " ++ show m ++ ", mi = " ++ show mi ++ ", d = " ++ show d ++ ", dd = " ++ show dd ++ ": " ++ show (elems mem)) False = undefined
    | otherwise = array (bounds mem) [(i, (if i == mi then 0 else v) + d + (if (i - mi - 1) `mod` w < dd then 1 else 0)) | (i, v) <- assocs mem]
    where
        -- find maximum and its index
        (m, mi, _) = Data.Foldable.foldl (\(m, i, j) v -> if v > m then (v, j, j+1) else (m, i, j+1)) (0, 0, 0) mem
        (l, h) = bounds mem
        w = h - l + 1
        --  how much bump should each element get
        d = m `div` w
        --  last index of the element which gets more bump
        dd = m `mod` w
    
