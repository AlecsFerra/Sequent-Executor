module Interpreter.SequentInterpreter where

import Type.Derivation
import Type.Sequent
import Type.Proposition
import Data.List

derive :: Sequent -> Derivation
derive s@(Sequent sx xd)
    | TTrue `elem` xd    = Axiom s "ax-tt"
    | FFalse `elem` sx   = Axiom s "ax-⊥"
    | compareList sx xd  = Axiom s "ax-id"
    | not $ allAtomOrNull sx = case sh of
        Not a      -> NormalDerivation s [derive (Sequent (tail sx) (a:xd))]                                  "¬-S"
        And a b    -> NormalDerivation s [derive (Sequent (a:b:tail sx) xd)]                                  "&-S"
        Or a b     -> NormalDerivation s [derive (Sequent (a:tail sx) xd), derive (Sequent (b:tail sx) xd)]   "∨-S"
        Imply a b  -> NormalDerivation s [derive (Sequent (tail sx) (a:xd)), derive (Sequent (b:tail sx) xd)] "→-S"
        a@(Atom _) -> NormalDerivation s [derive (Sequent (tail sx ++ [a]) xd)]                               "sc-S"
    | not $ allAtomOrNull xd = case dh of
        Not a      -> NormalDerivation s [derive (Sequent (a:sx) (tail xd))]                                  "¬-D"
        And a b    -> NormalDerivation s [derive (Sequent sx (a:tail xd)), derive (Sequent sx (a:tail xd))]   "&-D"
        Or a b     -> NormalDerivation s [derive (Sequent sx (a:b:tail xd))]                                  "∨-D"
        Imply a b  -> NormalDerivation s [derive (Sequent (a:sx) (b:tail xd))]                                "→-D"
        a@(Atom _) -> NormalDerivation s [derive (Sequent sx (tail xd ++ [a]))]                               "sc-D"
    | otherwise = NotDerivable s
    where 
      sh = head sx
      dh = head xd
    

allAtomOrNull :: [Proposition] -> Bool
allAtomOrNull []     = True
allAtomOrNull (x:xs) = case x of
    Atom _ -> allAtomOrNull xs
    _      -> False

compareList :: (Eq a) => [a] -> [a] -> Bool
compareList a = not . null . intersect a