module Glushkov
    ( DFA(..)
    , DFAState
    , DisplayTransition
    , buildDFA
    , describeDFA
    ) where

import Data.List (intercalate, nub, sort)
import Regex

type DFAState = [Int]
type Transition = (DFAState, Char, DFAState)
type DisplayTransition = (String, Char, String)

data DFA = DFA
    { dfaAlphabet :: [Char]
    , dfaStates :: [DFAState]
    , dfaStart :: DFAState
    , dfaFinals :: [DFAState]
    , dfaTransitions :: [Transition]
    , dfaStateNames :: [(DFAState, String)]
    , dfaPositions :: [(Int, Char)]
    , dfaNullable :: Bool
    , dfaFirst :: [Int]
    , dfaLast :: [Int]
    , dfaFollow :: [(Int, [Int])]
    }

normalize :: [Int] -> [Int]
normalize = nub . sort

nullable :: MarkedRegex -> Bool
nullable MEmpty = False
nullable MEps = True
nullable (MSym _ _) = False
nullable (MAlt a b) = nullable a || nullable b
nullable (MSeq a b) = nullable a && nullable b
nullable (MStar _) = True

firstPos :: MarkedRegex -> [Int]
firstPos MEmpty = []
firstPos MEps = []
firstPos (MSym n _) = [n]
firstPos (MAlt a b) = normalize (firstPos a ++ firstPos b)
firstPos (MSeq a b)
    | nullable a = normalize (firstPos a ++ firstPos b)
    | otherwise = firstPos a
firstPos (MStar a) = firstPos a

lastPos :: MarkedRegex -> [Int]
lastPos MEmpty = []
lastPos MEps = []
lastPos (MSym n _) = [n]
lastPos (MAlt a b) = normalize (lastPos a ++ lastPos b)
lastPos (MSeq a b)
    | nullable b = normalize (lastPos a ++ lastPos b)
    | otherwise = lastPos b
lastPos (MStar a) = lastPos a

followPos :: MarkedRegex -> [(Int, [Int])]
followPos MEmpty = []
followPos MEps = []
followPos (MSym _ _) = []
followPos (MAlt a b) = mergeFollow (followPos a) (followPos b)
followPos (MSeq a b) =
    let localPairs = [(x, firstPos b) | x <- lastPos a]
    in mergeFollow (mergeFollow (followPos a) (followPos b)) localPairs
followPos (MStar a) =
    let localPairs = [(x, firstPos a) | x <- lastPos a]
    in mergeFollow (followPos a) localPairs

mergeFollow :: [(Int, [Int])] -> [(Int, [Int])] -> [(Int, [Int])]
mergeFollow xs [] = xs
mergeFollow xs ((n, ys) : rest) = mergeFollow (addFollow xs n ys) rest

addFollow :: [(Int, [Int])] -> Int -> [Int] -> [(Int, [Int])]
addFollow [] n ys = [(n, normalize ys)]
addFollow ((m, xs) : rest) n ys
    | m == n = (m, normalize (xs ++ ys)) : rest
    | otherwise = (m, xs) : addFollow rest n ys

symbolAt :: [(Int, Char)] -> Int -> Char
symbolAt [] _ = error "unknown position"
symbolAt ((n, c) : rest) pos
    | n == pos = c
    | otherwise = symbolAt rest pos

lookupFollow :: [(Int, [Int])] -> Int -> [Int]
lookupFollow [] _ = []
lookupFollow ((n, xs) : rest) pos
    | n == pos = xs
    | otherwise = lookupFollow rest pos

stateName :: DFAState -> String
stateName [] = "{}"
stateName xs = "{" ++ intercalate "," (map show xs) ++ "}"

finalState :: Bool -> [Int] -> DFAState -> Bool
finalState isNullable lasts state
    | null state = isNullable
    | otherwise = not (null [x | x <- state, x `elem` lasts])

alphabetFrom :: [(Int, Char)] -> [Char]
alphabetFrom positions = normalizeChars [c | (_, c) <- positions]

normalizeChars :: [Char] -> [Char]
normalizeChars = nub . sort

moveFromStart :: [Int] -> [(Int, Char)] -> Char -> DFAState
moveFromStart firsts positions letter =
    normalize [p | p <- firsts, symbolAt positions p == letter]

moveFromState :: [(Int, [Int])] -> [(Int, Char)] -> DFAState -> Char -> DFAState
moveFromState follow positions state letter =
    normalize
        [ q
        | p <- state
        , q <- lookupFollow follow p
        , symbolAt positions q == letter
        ]

buildDFA :: Regex -> DFA
buildDFA regex =
    let (marked, positions) = markRegex regex
        firsts = firstPos marked
        lasts = lastPos marked
        follow = followPos marked
        isNullable = nullable marked
        alphabet = alphabetFrom positions
        startState = []
        statesAndTrans = explore alphabet positions follow firsts [startState] [] []
        allStates = fst statesAndTrans
        transitions = snd statesAndTrans
        finals = [state | state <- allStates, finalState isNullable lasts state]
        names = zip allStates ["q" ++ show n | n <- [0 :: Int ..]]
    in DFA
        { dfaAlphabet = alphabet
        , dfaStates = allStates
        , dfaStart = startState
        , dfaFinals = finals
        , dfaTransitions = transitions
        , dfaStateNames = names
        , dfaPositions = positions
        , dfaNullable = isNullable
        , dfaFirst = firsts
        , dfaLast = lasts
        , dfaFollow = sortFollow follow
        }
    where
        explore :: [Char] -> [(Int, Char)] -> [(Int, [Int])] -> [Int] -> [DFAState] -> [DFAState] -> [Transition] -> ([DFAState], [Transition])
        explore _ _ _ _ [] done transitions = (reverse done, reverse transitions)
        explore alphabet positions follow firsts (state : queue) done transitions
            | state `elem` done = explore alphabet positions follow firsts queue done transitions
            | otherwise =
                let currentMoves = transitionsFrom alphabet positions follow firsts state
                    nextStates = [target | (_, _, target) <- currentMoves, not (target `elem` done), not (target `elem` queue)]
                in explore alphabet positions follow firsts (queue ++ nextStates) (state : done) (reverse currentMoves ++ transitions)

transitionsFrom :: [Char] -> [(Int, Char)] -> [(Int, [Int])] -> [Int] -> DFAState -> [Transition]
transitionsFrom alphabet positions follow firsts state =
    [(state, c, move c) | c <- alphabet]
    where
        move letter
            | null state = moveFromStart firsts positions letter
            | otherwise = moveFromState follow positions state letter

sortFollow :: [(Int, [Int])] -> [(Int, [Int])]
sortFollow [] = []
sortFollow ((n, xs) : rest) = insertFollow (n, normalize xs) (sortFollow rest)

insertFollow :: (Int, [Int]) -> [(Int, [Int])] -> [(Int, [Int])]
insertFollow pair [] = [pair]
insertFollow pair@(n, _) (x@(m, ys) : rest)
    | n < m = pair : x : rest
    | n == m = pair : rest
    | otherwise = x : insertFollow pair rest

lookupName :: [(DFAState, String)] -> DFAState -> String
lookupName [] _ = error "unknown state"
lookupName ((state, name) : rest) target
    | state == target = name
    | otherwise = lookupName rest target

describeState :: [(DFAState, String)] -> DFAState -> String
describeState names state = lookupName names state ++ " = " ++ stateName state

describeTransition :: [(DFAState, String)] -> Transition -> DisplayTransition
describeTransition names (fromState, letter, toState) =
    (lookupName names fromState, letter, lookupName names toState)

describeDFA :: DFA -> ([String], [DisplayTransition])
describeDFA dfa =
    let stateLines = map (describeState (dfaStateNames dfa)) (dfaStates dfa)
        transLines = map (describeTransition (dfaStateNames dfa)) (dfaTransitions dfa)
    in (stateLines, transLines)
