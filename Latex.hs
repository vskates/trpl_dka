module Latex
    ( buildLatexDocument
    ) where

import Data.List (intercalate)
import Glushkov

buildLatexDocument :: DFA -> String -> String
buildLatexDocument dfa sourceRegex =
    unlines $
        [ "\\documentclass[a4paper]{article}"
        , "\\usepackage[T2A]{fontenc}"
        , "\\usepackage[utf8]{inputenc}"
        , "\\usepackage[russian]{babel}"
        , "\\usepackage{amsmath}"
        , "\\usepackage{tikz}"
        , "\\usetikzlibrary{automata,positioning}"
        , "\\usepackage[margin=2cm]{geometry}"
        , "\\begin{document}"
        , "\\section*{ДКА для регулярного выражения}"
        , "Исходное регулярное выражение: $" ++ escapeText sourceRegex ++ "$."
        , ""
        , "\\subsection*{Состояния}"
        ]
        ++ map (\line -> line ++ "\\\\") (stateDescriptions dfa)
        ++ [ ""
           , "\\subsection*{Переходы}"
           ]
        ++ map (\line -> line ++ "\\\\") (transitionDescriptions dfa)
        ++ [ ""
           , "\\subsection*{Диаграмма}"
           , "\\begin{center}"
           , "\\begin{tikzpicture}[->, >=stealth, shorten >=1pt, auto, node distance=2.8cm, semithick]"
           ]
        ++ nodeLines dfa
        ++ edgeLines dfa
        ++ [ "\\end{tikzpicture}"
           , "\\end{center}"
           , "\\end{document}"
           ]

stateDescriptions :: DFA -> [String]
stateDescriptions dfa =
    [ name ++ " = $" ++ prettyState state ++ "$"
    | (state, name) <- dfaStateNames dfa
    ]

transitionDescriptions :: DFA -> [String]
transitionDescriptions dfa =
    [ "$" ++ fromName ++ " \\xrightarrow{" ++ [letter] ++ "} " ++ toName ++ "$"
    | (fromName, letter, toName) <- snd (describeDFA dfa)
    ]

prettyState :: [Int] -> String
prettyState [] = "\\emptyset"
prettyState xs = "\\{" ++ intercalate "," (map show xs) ++ "\\}"

nodeLines :: DFA -> [String]
nodeLines dfa =
    [ makeNode index state name
    | (index, (state, name)) <- zip [0 :: Int ..] (dfaStateNames dfa)
    ]
    where
        finals = dfaFinals dfa
        makeNode index state name =
            let x = show (4 * (index `mod` 4))
                y = show ((-3) * (index `div` 4))
                attrs = nodeAttrs index state finals
            in "\\node[" ++ attrs ++ "] (" ++ name ++ ") at (" ++ x ++ "," ++ y ++ ") {$" ++ prettyState state ++ "$};"

nodeAttrs :: Int -> [Int] -> [[Int]] -> String
nodeAttrs index state finals
    | index == 0 && state `elem` finals = "state, initial, accepting"
    | index == 0 = "state, initial"
    | state `elem` finals = "state, accepting"
    | otherwise = "state"

edgeLines :: DFA -> [String]
edgeLines dfa =
    [ "\\path (" ++ fromName ++ ") edge" ++ edgeOption fromName toName ++ " node {$" ++ [letter] ++ "$} (" ++ toName ++ ");"
    | (fromName, letter, toName) <- snd (describeDFA dfa)
    ]

edgeOption :: String -> String -> String
edgeOption fromName toName
    | fromName == toName = "[loop above]"
    | otherwise = "[bend left=12]"

escapeText :: String -> String
escapeText [] = []
escapeText (x : xs)
    | x == '\\' = "\\textbackslash{}" ++ escapeText xs
    | x == '{' = "\\{" ++ escapeText xs
    | x == '}' = "\\}" ++ escapeText xs
    | x == '_' = "\\_" ++ escapeText xs
    | otherwise = x : escapeText xs
