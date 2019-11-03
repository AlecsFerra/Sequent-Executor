module Interpreter.SequentInterpreter where

import Type.Derivation
import Type.Sequent
import Type.Proposition
import Data.List

derive :: Sequent -> Derivation
derive s@(Sequent sx xd)
    | TTrue `elem` xd    = Axiom "ax-tt"
    | FFalse `elem` sx   = Axiom "ax-⊥"
    | compareList sx xd  = Axiom "ax-id"
    | not $ allAtomOrNull sx = case sh of
        Not a      -> NormalDerivation s [derive (Sequent (tail sx) (a:xd))]                                   ""
        And a b    -> NormalDerivation s [derive (Sequent ( a:b:tail sx) xd)]                                  ""
        Or a b     -> NormalDerivation s [derive (Sequent ( a:tail sx) xd),  derive (Sequent ( b:tail sx) xd)] ""
        Imply a b  -> NormalDerivation s [derive (Sequent (tail sx) (a:xd)), derive (Sequent ( b:tail sx) xd)] ""
        a@(Atom _) -> NormalDerivation s [derive (Sequent (tail sx ++ [a]) xd)]                                ""
    | not $ allAtomOrNull xd = case dh of
        Not a      -> NormalDerivation s [] ""
        And a b    -> NormalDerivation s [] ""
        Or a b     -> NormalDerivation s [] ""
        a@(Atom _) -> NormalDerivation s [] ""
    | otherwise = NotDerivable 
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