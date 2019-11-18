module Interpreter.SequentInterpreter (
  derive
) where

import Type.Derivation
import Type.Sequent
import Type.Proposition
import Data.List

derive :: Sequent -> Derivation
derive s@(Sequent sx dx)
    | TTrue  `elem` dx   = Axiom s "ax-tt"
    | FFalse `elem` sx   = Axiom s "ax-⊥"
    | compareList sx dx  = Axiom s "ax-id"
    | not $ allAtomTTrueFFalseOrNull sx = case sh of
        Not a      -> NormalDerivation s [derive (Sequent (tail sx) (a:dx))]                                  $ Action LLeft Negate
        And a b    -> NormalDerivation s [derive (Sequent (a:b:tail sx) dx)]                                  $ Action LLeft Anded
        Or a b     -> NormalDerivation s [derive (Sequent (a:tail sx) dx),   derive (Sequent (b:tail sx) dx)] $ Action LLeft Ored
        Imply a b  -> NormalDerivation s [derive (Sequent (tail sx) (a:dx)), derive (Sequent (b:tail sx) dx)] $ Action LLeft Implied
        a@(Atom _) -> NormalDerivation s [derive (Sequent (tail sx ++ [a]) dx)]                               $ Action LLeft Swap
    | not $ allAtomTTrueFFalseOrNull dx = case dh of
        Not a      -> NormalDerivation s [derive (Sequent (a:sx) (tail dx))]                                  $ Action RRight Negate
        And a b    -> NormalDerivation s [derive (Sequent sx (a:tail dx)),   derive (Sequent sx (b:tail dx))] $ Action RRight Anded
        Or a b     -> NormalDerivation s [derive (Sequent sx (a:b:tail dx))]                                  $ Action RRight Ored
        Imply a b  -> NormalDerivation s [derive (Sequent (a:sx) (b:tail dx))]                                $ Action RRight Implied
        a@(Atom _) -> NormalDerivation s [derive (Sequent sx (tail dx ++ [a]))]                               $ Action RRight Swap
    | otherwise = NotDerivable s
    where 
      sh = head sx
      dh = head dx

allAtomTTrueFFalseOrNull :: [Proposition] -> Bool
allAtomTTrueFFalseOrNull []     = True
allAtomTTrueFFalseOrNull (x:xs) = case x of
    Atom _ -> allAtomTTrueFFalseOrNull xs
    TTrue  -> allAtomTTrueFFalseOrNull xs
    FFalse -> allAtomTTrueFFalseOrNull xs
    _      -> False

compareList :: (Eq a) => [a] -> [a] -> Bool
compareList a = not . null . intersect a