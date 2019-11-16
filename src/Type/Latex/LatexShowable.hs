module Type.Latex.LatexShowable where

import Data.List
import Type.Sequent
import Type.Proposition
import Type.Derivation

class LatexShowable a where
  showLatex :: a -> String

instance LatexShowable Sequent where
  showLatex (Sequent [] []) = "\\vdash"
  showLatex (Sequent p1 []) = intercalate ", " (map showLatex (reverse p1)) ++ " \\vdash"
  showLatex (Sequent [] p2) = "\\vdash " ++ intercalate ", "  (map showLatex p2)
  showLatex (Sequent p1 p2) = intercalate ", " (map showLatex (reverse p1)) ++ " \\vdash " ++ intercalate ", " (map showLatex p2)

instance LatexShowable Proposition where
    showLatex TTrue = "tt"
    showLatex FFalse = "\\perp"
    showLatex (Atom c) = [c]
    showLatex (Not p) =  "\\neg " ++ showBracketIfNecessaryLatex p
    showLatex (And p1 p2) = showBracketIfNecessaryLatex p1 ++ " \\& "  ++ showBracketIfNecessaryLatex p2
    showLatex (Or p1 p2) = showBracketIfNecessaryLatex p1 ++ " ∨ "  ++ showBracketIfNecessaryLatex p2
    showLatex (Imply p1 p2) = showLatex p1 ++ " \\rightarrow " ++ showLatex p2

instance LatexShowable Derivation where
  showLatex p = "\\begin{prooftree}\n" ++ intercalate "\n" (reverse (latexDerivationComponent p)) ++ "\n\\end{prooftree}"

latexDerivationComponent :: Derivation -> [String]
latexDerivationComponent (NotDerivable s)         = ["\\AxiomC{$ " ++ showLatex s ++ " $}"]
latexDerivationComponent (Axiom s t)              = ["\\UnaryInfC{$ " ++ showLatex s ++ " $}"] ++  ["\\noLine"] ++ ["\\AxiomC{ " ++ show t ++ "}"]
latexDerivationComponent (NormalDerivation s c t) = [provideRightInf c ++ showLatex s ++  " $}"] ++ ["\\RightLabel{" ++ show t ++ "}"] ++ concatMap latexDerivationComponent c

provideRightInf :: [a] -> String
provideRightInf l
  | length l == 1 = "\\UnaryInfC{$ "
  | length l == 2 = "\\BinaryInfC{$ "
  | otherwise     = error "Unexpected len"
  

showBracketIfNecessaryLatex :: Proposition -> String
showBracketIfNecessaryLatex (Atom id) = [id]
showBracketIfNecessaryLatex TTrue = showLatex TTrue
showBracketIfNecessaryLatex FFalse = showLatex FFalse
showBracketIfNecessaryLatex p = "(" ++ showLatex p ++ ")"