module Main where

import Type.Proposition
import Type.Sequent
import Text.Parsec
import Parser.SequentParser

main :: IO ()
main = interact (unlines . (map parseProp) . lines)

parseProp :: String -> String
parseProp s = case ret of
    Left e -> "error: " ++ show e
    Right n -> "parsed: " ++ show n
  where
    ret = parse parseSequent "" s

{-}    print (Sequent [] [Atom 'C'])
    print (Sequent [And (Atom 'C') (Atom 'C')] [])
    print (Sequent [Not (Or (Atom 'C') TTrue), Or (Atom 'C') TTrue] [Imply (Atom 'C') FFalse])
    print (Sequent [] [])
    -}