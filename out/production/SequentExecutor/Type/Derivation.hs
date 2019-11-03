module Type.Derivation where

import Type.Sequent

data Derivation = NormalDerivation Sequent [Derivation] String
                | Axiom String
                | NotDerivable
                deriving (Eq, Show)