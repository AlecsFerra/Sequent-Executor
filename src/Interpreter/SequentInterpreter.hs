module Interpreter.SequentInterpreter where

import Type.Derivation
import Type.Sequent
import Type.Proposition
import Data.List

derive :: Bool -> Sequent -> Derivation
derive mode s@(Sequent sx xd)
    | TTrue `elem` xd    = Axiom s "ax-tt"
    | FFalse `elem` sx   = Axiom s "ax-⊥"
    | compareList sx xd  = Axiom s "ax-id"
    | not $ allAtomTTrueFFalseOrNull sx = case sh of
        Not a      -> NormalDerivation s [derive mode (Sequent (tail sx) (a:xd))]                                       (if mode then "$ \neg -S$"       else "¬-S")
        And a b    -> NormalDerivation s [derive mode (Sequent (a:b:tail sx) xd)]                                       (if mode then "$ and -S$"         else "&-S")
        Or a b     -> NormalDerivation s [derive mode (Sequent (a:tail sx) xd), derive mode (Sequent (b:tail sx) xd)]   "∨-S"
        Imply a b  -> NormalDerivation s [derive mode (Sequent (tail sx) (a:xd)), derive mode (Sequent (b:tail sx) xd)] (if mode then "$ \rightarrow -S$" else "→-S")
        a@(Atom _) -> NormalDerivation s [derive mode (Sequent (tail sx ++ [a]) xd)]                                    "sc-S"
    | not $ allAtomTTrueFFalseOrNull xd = case dh of
        Not a      -> NormalDerivation s [derive mode (Sequent (a:sx) (tail xd))]                                       (if mode then "$ \neg -D$"       else "¬-D")
        And a b    -> NormalDerivation s [derive mode (Sequent sx (a:tail xd)), derive mode (Sequent sx (b:tail xd))]   (if mode then "$ and -D$"         else "&-D")
        Or a b     -> NormalDerivation s [derive mode (Sequent sx (a:b:tail xd))]                                       "∨-D"
        Imply a b  -> NormalDerivation s [derive mode (Sequent (a:sx) (b:tail xd))]                                     (if mode then "$ \rightarrow -D$" else "→-D")
        a@(Atom _) -> NormalDerivation s [derive mode (Sequent sx (tail xd ++ [a]))]                                    "sc-D"
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