module Type.Derivation where

import Type.Sequent

data Derivation = NormalDerivation Sequent [Derivation] String
                | Axiom Sequent String
                | NotDerivable Sequent
                deriving (Eq, Show)

isTautology :: Derivation -> Bool
isTautology (Axiom _ _)               = True
isTautology (NotDerivable _)          = False
isTautology (NormalDerivation _ ds _) = all isTautology ds