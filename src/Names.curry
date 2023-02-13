-----------------------------------------------------------------------
--- Operations to change names of the original program into names
--- used in the target program
-----------------------------------------------------------------------
module Names where

import Data.Char  ( isAlphaNum )
import Data.List  ( intercalate )
import Data.Maybe ( fromJust, isJust )

genCorrectIdentifier :: String -> String
genCorrectIdentifier []     = error "genCorrectIdentifier: empty identifier"
genCorrectIdentifier (c:cs)
  | all opChar (c:cs) = c:cs
  | otherwise         = replaceNonIdChars "" "" (c:cs)
 where
  opChar = (`elem` "!#$%&*+./<=>?@\\^|-~")

showOpChar :: Char -> String
showOpChar c = case c of
  '_' -> "_" --"underscore" TODO: Can this lead to a name clash?
  '~' -> "tilde"
  '!' -> "bang"
  '@' -> "at"
  '#' -> "hash"
  '$' -> "dollar"
  '%' -> "percent"
  '^' -> "caret"
  '&' -> "ampersand"
  '*' -> "star"
  '+' -> "plus"
  '-' -> "minus"
  '=' -> "eq"
  '<' -> "lt"
  '>' -> "gt"
  '?' -> "qmark"
  '.' -> "dot"
  '/' -> "slash"
  '|' -> "bar"
  '\\' ->"backslash"
  ':' -> "colon"
  '(' -> "oparen"
  ')' -> "cparen"
  '[' -> "obracket"
  ']' -> "cbracket"
  ',' -> "comma"
  '\'' -> "tick"
  _   -> error $ "unexpected symbol: " ++ show c

-- | replaces characters that are not valid haskell identifiers,
-- | if there were no characters replaced, the first prefix,
-- | otherwise the snd prefix ist prepended
replaceNonIdChars :: String -> String -> String -> String
replaceNonIdChars pfxNonOp pfxOp str = case strings of
  []  -> error "replaceNonIdChars: empty identifier"
  [s] -> if isAlphaNum (head str)
            then pfxNonOp ++ s
            else pfxOp    ++ s
  _   -> pfxOp ++ intercalate "_" strings
 where strings       = separateAndReplace isIdentChar showOpChar str
       isIdentChar c = isAlphaNum c || c == '_' || c == '\''

separateAndReplace :: (a -> Bool) -> (a -> [a]) -> [a] -> [[a]]
separateAndReplace pred f list = case rest of
  [] -> case sep of
    [] -> []
    _  -> [sep]
  (x:xs) -> case sep of
    [] -> f x : separateAndReplace pred f xs
    _  -> sep : f x : separateAndReplace pred f xs
 where (sep,rest) = break  (not . pred) list

