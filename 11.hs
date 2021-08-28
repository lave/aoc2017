import Data.List

import Common

data Dir = Nw | N | Ne | Se | S | Sw
    deriving Show

main = do
    line <- readSingleLineFile "11.input"
    let path = parse line
    --putStrLn $ show path
    let (final, farest) = solve path
    putStrLn $ show $ final
    putStrLn $ show $ farest


parse line = map parseDir $ words $ map (\c -> if c == ',' then ' ' else c) line
    where
        parseDir dir =
            case dir of
                "nw" -> Nw
                "n"  -> N
                "ne" -> Ne
                "se" -> Se
                "s"  -> S
                "sw" -> Sw


solve path = (last dists, maximum dists)
    where
        coords = scanl' move (0, 0) path
        dists = map (hexDist (0, 0)) coords

        move (x, y) d =
            case d of
                Nw -> (x - 1, y + 1)
                N  -> (x, y + 2)
                Ne -> (x + 1, y + 1)
                Se -> (x + 1, y - 1)
                S  -> (x, y - 2)
                Sw -> (x - 1, y - 1)

hexDist (x1, y1) (x2, y2) =
    (abs (x1 - x2) + abs (y1 - y2)) `div` 2
