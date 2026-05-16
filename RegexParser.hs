module RegexParser
    ( Parser(..)
    , parseRegex
    ) where

import Control.Applicative (Alternative(..))
import Control.Monad (ap, liftM, MonadPlus)
import Data.Char (isAlphaNum, isSpace)
import Regex

newtype Parser a = Parser { runParse :: String -> Maybe (a, String) }

pureP :: a -> Parser a
pureP value = Parser $ \inp -> Just (value, inp)

zero :: Parser a
zero = Parser $ const Nothing

item :: Parser Char
item = Parser $ \inp ->
    case inp of
        [] -> Nothing
        (x : xs) -> Just (x, xs)

bindP :: Parser a -> (a -> Parser b) -> Parser b
bindP p f = Parser $ \inp ->
    case runParse p inp of
        Nothing -> Nothing
        Just (value, rest) -> runParse (f value) rest

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure = pureP
    (<*>) = ap

instance Monad Parser where
    (>>=) = bindP

instance Alternative Parser where
    empty = zero
    Parser left <|> Parser right = Parser $ \inp ->
        case left inp of
            Nothing -> right inp
            answer -> answer

instance MonadPlus Parser

sat :: (Char -> Bool) -> Parser Char
sat f = do
    c <- item
    if f c then pure c else empty

charP :: Char -> Parser Char
charP c = sat (== c)

star1 :: Parser a -> Parser [a]
star1 p = do
    x <- p
    xs <- star p
    return (x : xs)

star :: Parser a -> Parser [a]
star p = star1 p <|> pure []

spaces :: Parser String
spaces = star (sat isSpace)

token :: Parser a -> Parser a
token p = spaces *> p <* spaces

symbolChar :: Parser Char
symbolChar = sat ok
    where
        ok c = isAlphaNum c && c /= '0' && c /= '1'

regexP :: Parser Regex
regexP = altP

altP :: Parser Regex
altP = do
    first <- concatP
    rest <- star (token (charP '|') *> concatP)
    return (foldl Alt first rest)

concatP :: Parser Regex
concatP = do
    pieces <- star1 postfixP
    return (foldl1 Seq pieces)

postfixP :: Parser Regex
postfixP = do
    atom <- atomP
    stars <- star (token (charP '*'))
    return (applyStars atom stars)

applyStars :: Regex -> String -> Regex
applyStars regex [] = regex
applyStars regex (_ : xs) = applyStars (Star regex) xs

atomP :: Parser Regex
atomP =
        parenP
    <|> epsilonP
    <|> emptyP
    <|> symbolP

parenP :: Parser Regex
parenP = do
    token (charP '(')
    r <- regexP
    token (charP ')')
    return r

epsilonP :: Parser Regex
epsilonP = token (charP '1') *> pure Eps

emptyP :: Parser Regex
emptyP = token (charP '0') *> pure Empty

symbolP :: Parser Regex
symbolP = do
    c <- token symbolChar
    return (Sym c)

parseRegex :: String -> Maybe Regex
parseRegex inp =
    case runParse (spaces *> regexP <* spaces) inp of
        Just (regex, "") -> Just regex
        _ -> Nothing
