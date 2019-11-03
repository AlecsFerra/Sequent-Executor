module Type.Proposition where

data Proposition = TTrue
                 | FFalse
                 | Atom  Char
                 | And   Proposition Proposition
                 | Or    Proposition Proposition
                 | Not   Proposition
                 | Imply Proposition Proposition
                 deriving (Eq)

instance Show Proposition where
    show TTrue = "tt"
    show FFalse = "⊥"
    show (Atom c) = show c
    show (Not p) =  "¬" ++ showBracketIfNecessary p
    show (And p1 p2) = showBracketIfNecessary p1 ++ " & "  ++ showBracketIfNecessary p2
    show (Or p1 p2) = showBracketIfNecessary p1 ++ " ∨ "  ++ showBracketIfNecessary p2
    show (Imply p1 p2) = showBracketIfNecessary p1 ++ " → " ++ showBracketIfNecessary p2

showBracketIfNecessary :: Proposition -> String
showBracketIfNecessary (Atom id) = show id
showBracketIfNecessary TTrue = show TTrue
showBracketIfNecessary FFalse = show FFalse
showBracketIfNecessary p = "(" ++ show p ++ ")"