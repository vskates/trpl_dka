module Main where

import System.Environment (getArgs)

import Glushkov
import Latex
import Regex
import RegexParser

data Output = Output
    { outputRegex :: String
    , outputFile :: String
    }

defaultOutput :: Output
defaultOutput = Output
    { outputRegex = "(a|b)*abb"
    , outputFile = "dfa.tex"
    }

main :: IO ()
main = do
    args <- getArgs
    let options = parseArgs args defaultOutput
    case parseRegex (outputRegex options) of
        Nothing -> putStrLn "Не удалось разобрать регулярное выражение."
        Just regex -> do
            let dfa = buildDFA regex
                latexText = buildLatexDocument dfa (outputRegex options)
            writeFile (outputFile options) latexText
            printReport regex dfa (outputFile options)

parseArgs :: [String] -> Output -> Output
parseArgs [] options = options
parseArgs ("-o" : fileName : rest) options =
    parseArgs rest options { outputFile = fileName }
parseArgs (regex : rest) options =
    parseArgs rest options { outputRegex = regex }

printReport :: Regex -> DFA -> String -> IO ()
printReport regex dfa fileName = do
    putStrLn $ "Регулярное выражение: " ++ prettyRegex regex
    putStrLn $ "Алфавит: " ++ show (dfaAlphabet dfa)
    putStrLn $ "Nullable: " ++ show (dfaNullable dfa)
    putStrLn $ "FirstPos: " ++ show (dfaFirst dfa)
    putStrLn $ "LastPos: " ++ show (dfaLast dfa)
    putStrLn "Позиции:"
    mapM_ putStrLn (formatPositions (dfaPositions dfa))
    putStrLn "FollowPos:"
    mapM_ putStrLn (formatFollow (dfaFollow dfa))
    putStrLn "Состояния ДКА:"
    mapM_ putStrLn (fst (describeDFA dfa))
    putStrLn "Переходы ДКА:"
    mapM_ putStrLn (formatTransitions (snd (describeDFA dfa)))
    putStrLn $ "LaTeX записан в файл " ++ fileName

formatPositions :: [(Int, Char)] -> [String]
formatPositions positions =
    [ show n ++ " -> " ++ [c]
    | (n, c) <- positions
    ]

formatFollow :: [(Int, [Int])] -> [String]
formatFollow pairs =
    [ show n ++ " -> " ++ show xs
    | (n, xs) <- pairs
    ]

formatTransitions :: [DisplayTransition] -> [String]
formatTransitions pairs =
    [ fromName ++ " -" ++ [letter] ++ "-> " ++ toName
    | (fromName, letter, toName) <- pairs
    ]
