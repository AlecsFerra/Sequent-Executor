module Interpreter.SequentInterpreter where

import Type.Derivation
import Type.Sequent
import Type.Proposition
import Data.List

derive :: Sequent -> Derivation
derive s@(Sequent sx xd)
    | TTrue `elem` xd   = Axiom "ax-tt"
    | FFalse `elem` sx  = Axiom "ax-⊥"
    | compareList sx xd = Axiom "ax-id"
    | 
compareList :: (Eq a) => [a] -> [a] -> Bool
compareList a = not . null . intersect a