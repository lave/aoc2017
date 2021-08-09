import Common
import Data.Array
--import Data.Array.Diff
import Data.List

main = do
    content <- readFile "05.input"
    let mem = parse content
    putStrLn $ show $ solve mem (+ 1)
    putStrLn $ show $ solve mem (\v -> if v < 3 then v + 1 else v - 1)

parse c = listArray (0, length l - 1) l
    where
        l = map readInt $ lines c

solve mem modify = (length $ takeWhile ((>= 0) . snd) run) - 1
    where
        run = iterate next (mem, 0)
    
        next (mem, ip)
            | ip < 0 || ip >= n = (mem, -1)
            | otherwise = (mem', ip')
            where
                n = length mem
                v = mem ! ip
                ip' = ip + v
                mem' = mem // [(ip, modify v)]
