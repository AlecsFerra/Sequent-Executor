module Type.Derivation where

import Type.Sequent

data Derivation = NormalDerivation Sequent [Derivation] String
                | Axiom Sequent String
                | NotDerivable Sequent
                deriving (Eq)

isTautology :: Derivation -> Bool
isTautology (Axiom _ _)               = True
isTautology (NotDerivable _)          = False
isTautology (NormalDerivation _ ds _) = all isTautology ds

instance Show Derivation where
  show x = roseShow 0 x

roseShow :: Int -> Derivation -> String
roseShow d (NotDerivable s)         = "\n" ++ (mul "-" d) ++ " " ++ show s ++ " {NOT DERIVABLE}"
roseShow d (Axiom s t)              = "\n" ++ (mul "-" d) ++ " " ++ show s ++ " {Axiom: " ++ t ++ "}"
roseShow d (NormalDerivation s c t) = "\n" ++ (mul "-" d) ++ " " ++ show s ++ " {" ++ t ++ "}" ++ concat (map ( roseShow (d + 1)) c)

mul :: String -> Int -> String
mul s n = concat (replicate (n * 2) s)