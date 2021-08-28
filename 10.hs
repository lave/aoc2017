import Data.Array
import Data.Bits (xor)
import Data.Char
import Data.List
import Debug.Trace
import Numeric (showHex)

import Common

main = do
    line <- readSingleLineFile "10.input"
    putStrLn $ show $ solve1 line
    putStrLn $ solve2 line


rotate l = trd3 $ foldl' rotate' (0, 0, a0) l
    where
        n = 256
        a0 = listArray (0, n - 1) [0..]

        rotate' (pos, skip, a) l
            -- | trace ("\npos = " ++ show pos ++ ", l = " ++ show l ++ ": " ++ show (elems a) ++ " -> " ++ show (elems a')) False = undefined
            | otherwise = (pos', skip + 1, a')
            where
                pos' = (pos + l + skip) `mod` n
                a' = a // [((pos + i) `mod` n, a ! ((pos + l - 1 - i) `mod` n)) | i <- [0 .. l - 1]]


solve1 line = a ! 0 * a ! 1
    where
        l = read ("[" ++ line ++ "]") :: [Int]
        a = rotate l
        

solve2 line = str
    where
        l = (map ord line) ++ [17, 31, 73, 47, 23]
        l' = take (64 * length l) $ cycle l
        a = rotate l'
        vals = [map (a!) [i*16 .. i*16 + 15] | i <- [0..15]] :: [[Int]]
        str = concatMap (byte2hex . (foldl1' xor)) vals

        byte2hex b
            | b < 16 = '0' : showHex b ""
            | b < 256 = showHex b "" 
