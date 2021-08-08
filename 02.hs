import Common
import Data.List

main = do
    content <- readFile "02.input"
    let grid = parse content
    putStrLn $ show $ checksum minmax grid
    putStrLn $ show $ checksum divisible grid

parse = map (map readInt . words) . lines

checksum checksum_line =
    sum . map checksum_line

minmax l = maximum l - minimum l

divisible l = head [x `div` y | x <- l, y <- l, x > y && x `mod` y == 0]
