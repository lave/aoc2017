import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace

import Common

main = do
    content <- readFile "12.input"
    let graph = parse content
    --putStrLn $ show $ graph
    putStrLn $ show $ solve1 graph
    putStrLn $ show $ solve2 graph

parse content = Map.fromList $ map (head . parseLine) $ lines content
    where
        parseLine s = [(readInt id, map readInt $ words $ filter (/= ',') ids) |
            (id, s1) <- lex s,
            ("<->", ids) <- lex s1]


group graph id = reachable (Set.singleton id) (Set.singleton id)
    where
        reachable frontier visited
            -- | trace ("\n" ++ show frontier ++ " | " ++ show visited) False = undefined
            | Set.null frontier = visited
            | otherwise = reachable frontier' visited'
            where
                neighbours = Set.fromList $ concatMap (graph Map.!) $ Set.elems frontier
                frontier' = Set.difference neighbours visited
                visited' = Set.union visited frontier'


solve1 graph = Set.size $ group graph 0


solve2 graph = length $ groups (Set.fromList $ Map.keys graph) []
    where
        groups ids gs
            | Set.null ids = gs
            | otherwise = groups ids' (g : gs)
            where
                id = head $ Set.elems ids
                g = group graph id
                ids' = Set.difference ids g
