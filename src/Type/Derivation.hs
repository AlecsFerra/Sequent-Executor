module Type.Derivation where

import Type.Sequent

data Derivation = NormalDerivation Sequent [Derivation] String
                | Axiom Sequent String
                | NotDerivable Sequent
                deriving (Eq, Show)