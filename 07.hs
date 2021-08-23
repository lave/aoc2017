import Control.Monad
import Data.List
import Data.Maybe
import Data.Either
import Debug.Trace

import Common

main = do
    content <- readFile "07.input"
    let tree = parse content
    --putStrLn $ show $ tree
    let root = solve1 tree
    putStrLn $ root
    putStrLn $ show $ solve2 tree root

parse = map (head . parseLine) . lines
    where
        parseLine s = [(name, weight, parseDeps mb deps) |
            (name, s1) <- lex s,
            ('(', s2) <- readsChar s1,
            (weight, s3) <- readsInt s2,
            (')', s4) <- readsChar s3,
            (mb, deps) <- maybeDo lex s4]
        parseDeps mb deps =
            case mb of
                Nothing -> []
                Just "->" -> words $ filter (/= ',') deps


solve1 tree = root
    where
        all = map fst3 tree
        deps = concatMap trd3 tree
        [root] = all \\ deps


solve2 :: [(String, Int, [String])] -> String -> Int
solve2 tree root = fromLeft_ $ solve root
    where
        --  left  - result (proper weight for the node with incorrect weight)
        --  right - pair: weight of node, weight of its subtree
        solve :: String -> Either Int (Int, Int)
        solve node
            -- | trace ("\nnode = " ++ node ++ ", weights = " ++ show weights) False = undefined
            | null deps = Right (w, w)
            | isLeft weights = Left $ fromLeft_ weights
            | otherwise = fmap (\((_, w'), _) -> (w, w + w' * length weights')) $ foldM acc (w0, w1) ws
            where
                (_, w, deps) = fromJust $ find ((== node) . fst3) tree
                weights = sequence $ map solve deps
                weights' = fromRight_ weights
                (w0 : w1 : ws) = weights'

--  result:
--      left  - result (proper weight for the node with incorrect weight)
--      right - two first weights (if all weights are correct)
acc :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Either Int ((Int, Int), (Int, Int))
acc xs@((w0, x0), (w1, x1)) (w, x)
    -- | trace ("\nxs = " ++ show xs ++ ", x = " ++ show (w, x)) False = undefined
    | x0 == x1 && x == x0 = Right xs
    | x0 == x1 && x /= x0 = Left (w + x0 - x)
    | x0 /= x1 && x == x0 = Left (w1 + x0 - x1)
    | x0 /= x1 && x == x1 = Left (w0 + x1 - x0)
