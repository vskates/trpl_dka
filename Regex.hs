module Regex
    ( Regex(..)
    , MarkedRegex(..)
    , markRegex
    , prettyRegex
    ) where

data Regex
    = Empty
    | Eps
    | Sym Char
    | Alt Regex Regex
    | Seq Regex Regex
    | Star Regex
    deriving (Eq, Show)

data MarkedRegex
    = MEmpty
    | MEps
    | MSym Int Char
    | MAlt MarkedRegex MarkedRegex
    | MSeq MarkedRegex MarkedRegex
    | MStar MarkedRegex
    deriving (Eq, Show)

prettyRegex :: Regex -> String
prettyRegex Empty = "0"
prettyRegex Eps = "1"
prettyRegex (Sym c) = [c]
prettyRegex (Alt a b) = "(" ++ prettyRegex a ++ "|" ++ prettyRegex b ++ ")"
prettyRegex (Seq a b) = "(" ++ prettyRegex a ++ prettyRegex b ++ ")"
prettyRegex (Star a) = "(" ++ prettyRegex a ++ ")*"

markRegex :: Regex -> (MarkedRegex, [(Int, Char)])
markRegex regex =
    let (marked, _, symbols) = go regex 1
    in (marked, symbols)
    where
        go :: Regex -> Int -> (MarkedRegex, Int, [(Int, Char)])
        go Empty n = (MEmpty, n, [])
        go Eps n = (MEps, n, [])
        go (Sym c) n = (MSym n c, n + 1, [(n, c)])
        go (Alt a b) n =
            let (a1, n1, xs) = go a n
                (b1, n2, ys) = go b n1
            in (MAlt a1 b1, n2, xs ++ ys)
        go (Seq a b) n =
            let (a1, n1, xs) = go a n
                (b1, n2, ys) = go b n1
            in (MSeq a1 b1, n2, xs ++ ys)
        go (Star a) n =
            let (a1, n1, xs) = go a n
            in (MStar a1, n1, xs)
