import Control.Monad
import Data.Array
import Data.Maybe
import Data.List
import Debug.Trace

import Common

data Op = Eq | Ne | Lt | Le | Gt | Ge
    deriving Show

main = do
    content <- readFile "08.input"
    let prg = numRegs $ parse content
    --putStrLn $ show $ prg
    let (m1, m2) = solve prg
    putStrLn $ show m1
    putStrLn $ show m2

parse = map (head . parseCmd) . lines
    where
        parseCmd s = [(reg, (if inc == "inc" then 1 else -1) * d, creg, parseOp cop, carg) |
            (reg, s1) <- lex s,
            (inc, s2) <- lex s1,
            (d, s3) <- readsInt s2,
            ("if", s4) <- lex s3,
            (creg, s5) <- lex s4,
            (cop, s6) <- lex s5,
            (carg, "") <- readsInt s6]
        parseOp op =
            case op of
                "==" -> Eq
                "!=" -> Ne
                "<"  -> Lt
                "<=" -> Le
                ">"  -> Gt
                ">=" -> Ge

numRegs prg = (length regs, map numRegs' prg)
    where
        numRegs' (reg, d, creg, cop, carg) = (numReg reg, d, numReg creg, cop, carg)
        numReg name = fromJust $ lookup name regs
        regs = zip (nub $ concatMap (\(reg, _, creg, _, _) -> [reg, creg]) prg) [0..]


solve (nregs, cmds) = (maximum $ elems regs, max_v)
    where
        (regs, max_v) = foldl' runCmd (regs0, 0) cmds
        regs0 = listArray (0, nregs - 1) $ repeat 0

        runCmd state@(regs, max_v) cmd@(reg, d, creg, cop, carg)
            -- | trace ("\ncmd: " ++ show cmd ++ ", regs" ++ (show $ elems regs)) False = undefined
            | cond = (regs // [(reg, v')], max max_v v')
            | otherwise = state
            where
                v' = (regs ! reg) + d
                cv = regs ! creg
                cond = case cop of
                    Eq -> cv == carg
                    Ne -> cv /= carg
                    Lt -> cv < carg
                    Le -> cv <= carg
                    Gt -> cv > carg
                    Ge -> cv >= carg
