module Parser.PropositionParser where

import Type.Proposition
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Expr


lexer :: TokenParser()
lexer = makeTokenParser (javaStyle { opStart  = oneOf "¬&∨→",
                                     opLetter = oneOf "¬&∨→" })

parseAtom :: Parser Proposition
parseAtom = do
    id <- charLiteral lexer
    return $ Atom id

parseFFalse :: Parser  Proposition
parseFFalse = do
    _ <- oneOf "⊥"
    return FFalse

parseTTrue :: Parser  Proposition
parseTTrue = do
    _ <- oneOf "t"
    return TTrue

parseGate :: Parser Proposition
parseGate = (flip buildExpressionParser) parseTerm $ [
        [ Prefix (reservedOp lexer "¬" >> return Not  ) ],
        [ Infix  (reservedOp lexer "&" >> return And  ) AssocLeft,
          Infix  (reservedOp lexer "∨" >> return Or   ) AssocLeft ],
        [ Infix  (reservedOp lexer "→" >> return Imply) AssocLeft ]
    ]

parseTerm :: Parser Proposition
parseTerm = parens lexer parseGate
                     <|> parseTTrue
                     <|> parseFFalse
                     <|> parseAtom

parseProposition :: Parser Proposition
parseProposition = do
  whiteSpace lexer
  n <- parseGate
  return n
                            