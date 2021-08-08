import Common
import Data.List

main = do
    content <- readFile "04.input"
    let passphrases = parse content
    putStrLn $ show $ length $ filter isValid1 passphrases
    putStrLn $ show $ length $ filter isValid2 passphrases

parse = map words . lines

isValid1 pp = length pp == length (nub pp)

isValid2 pp = isValid1 $ map sort pp
