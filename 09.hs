import Data.List

main = do
    content <- readFile "09.input"
    let [stream] = lines content    
    --putStrLn $ show $ stream
    let (scores, garbage) = solve stream
    putStrLn $ show $ scores
    putStrLn $ show $ garbage

data State = State {
    score :: Int,
    scores :: Int,
    garbage :: Int,
    isGarbage :: Bool,
    isEscaped :: Bool
} deriving (Show)

solve stream = (scores state, garbage state)
    where
        state = foldl' consume state0 stream
        state0 = State 0 0 0 False False
        consume state@(State score scores garbage isGarbage isEscaped) c
            | isEscaped = state { isEscaped = False }
            | not isGarbage && c == '{' = state { score = score + 1 }
            | not isGarbage && c == '}' = state { score = score - 1, scores = scores + score }
            | not isGarbage && c == '<' = state { isGarbage = True }
            | isGarbage && c == '>' = state { isGarbage = False }
            | isGarbage && c == '!' = state { isEscaped = True }
            | isGarbage = state { garbage = garbage + 1 }
            | otherwise = state
