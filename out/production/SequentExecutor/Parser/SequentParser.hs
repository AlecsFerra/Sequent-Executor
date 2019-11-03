module Parser.SequentParser where

import Parser.PropositionParser
import Type.Proposition
import Type.Sequent
import Text.Parsec
import Text.Parsec.String
import Data.List

parsePropositions :: Parser [Proposition]
parsePropositions = many parseProposition

parseSequent :: Parser Sequent
parseSequent = do
    l <- parsePropositions
    _ <- many space
    _ <- oneOf "⊢"
    _ <- many space
    r <- parsePropositions
    return (Sequent (reverse l) r)