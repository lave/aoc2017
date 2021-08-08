import Common
import Data.List

main = do
    content <- readFile "01.input"
    let l = parse content
    putStrLn $ show $ captcha 1 l
    putStrLn $ show $ captcha (length l `div` 2) l

parse = map (readInt . return) . head . lines

captcha n l = sum $ zipWith (\x y -> if x == y then x else 0) l (drop n $ cycle l)
