module Main where

import Type.Proposition
import Type.Sequent
import Type.Derivation
import Text.Parsec
import Parser.SequentParser
import Interpreter.SequentInterpreter
import Type.Latex.LatexShowable
import System.Environment.Blank (getArgs)

main :: IO ()
main = do
  ins <- getLine
  case rawParse ins of
    Left e    -> putStrLn $ "Error: " ++ show e
    Right seq -> do
      putStrLn (derivationPrinter derived)
      case tau of
        True  -> putStrLn "The inserted sequent is a tautology"
        False -> do
          putStrLn "The inserted sequent wasn't a tautology"
          putStrLn "Now evaluating the opposite"
          putStrLn (derivationPrinter derivedOpposite)
          case para of
            True -> putStrLn "The inserted sequent is a paradox"
            False -> putStrLn "The inserted sequent is an opinion"
     where
      derivationPrinter = showLatex
      derived = derive seq
      tau = isTautology derived
      oppositeSeq = opposite seq
      derivedOpposite = derive oppositeSeq
      para = isTautology derivedOpposite
  
rawParse :: String -> Either ParseError Sequent
rawParse = parse parseSequent ""



{-}    print (Sequent [] [Atom 'C'])
    print (Sequent [And (Atom 'C') (Atom 'C')] [])
    print (Sequent [Not (Or (Atom 'C') TTrue), Or (Atom 'C') TTrue] [Imply (Atom 'C') FFalse])
    print (Sequent [] [])
    -}