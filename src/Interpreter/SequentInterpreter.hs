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
    | not $ allAtomTTrueFFalseOrNull sx = case sh of
        Not a      -> NormalDerivation s [derive (Sequent (tail sx) (a:xd))]                                  $ Action LLeft Negate
        And a b    -> NormalDerivation s [derive (Sequent (a:b:tail sx) xd)]                                  $ Action LLeft Anded
        Or a b     -> NormalDerivation s [derive (Sequent (a:tail sx) xd), derive (Sequent (b:tail sx) xd)]   $ Action LLeft Ored
        Imply a b  -> NormalDerivation s [derive (Sequent (tail sx) (a:xd)), derive (Sequent (b:tail sx) xd)] $ Action LLeft Implied
        a@(Atom _) -> NormalDerivation s [derive (Sequent (tail sx ++ [a]) xd)]                               $ Action LLeft Swap
    | not $ allAtomTTrueFFalseOrNull xd = case dh of
        Not a      -> NormalDerivation s [derive (Sequent (a:sx) (tail xd))]                                  $ Action RRight Negate
        And a b    -> NormalDerivation s [derive (Sequent sx (a:tail xd)), derive (Sequent sx (b:tail xd))]   $ Action RRight Anded
        Or a b     -> NormalDerivation s [derive (Sequent sx (a:b:tail xd))]                                  $ Action RRight Ored
        Imply a b  -> NormalDerivation s [derive (Sequent (a:sx) (b:tail xd))]                                $ Action RRight Implied
        a@(Atom _) -> NormalDerivation s [derive (Sequent sx (tail xd ++ [a]))]                               $ Action RRight Swap
    | otherwise = NotDerivable s
    where 
      sh = head sx
      dh = head xd

allAtomTTrueFFalseOrNull :: [Proposition] -> Bool
allAtomTTrueFFalseOrNull []     = True
allAtomTTrueFFalseOrNull (x:xs) = case x of
    Atom _ -> allAtomTTrueFFalseOrNull xs
    TTrue  -> allAtomTTrueFFalseOrNull xs
    FFalse -> allAtomTTrueFFalseOrNull xs
    _      -> False

compareList :: (Eq a) => [a] -> [a] -> Bool
compareList a = not . null . intersect a