import Data.List

import Common

main = do
    line <- readSingleLineFile "01.input"
    let l = parse line
    putStrLn $ show $ captcha 1 l
    putStrLn $ show $ captcha (length l `div` 2) l

parse = map (readInt . return)

captcha n l = sum $ zipWith (\x y -> if x == y then x else 0) l (drop n $ cycle l)
