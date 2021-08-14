module Common where
import qualified Data.Set

readInt s = read s :: Int


firstDup l = firstDup' l Data.Set.empty 0
    where
        firstDup' [] _ _ = Nothing
        firstDup' (x : xs) xs' i
            | Data.Set.member x xs' = Just (i, x)
            | otherwise = firstDup' xs (Data.Set.insert x xs') (i + 1)

