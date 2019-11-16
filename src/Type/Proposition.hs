module Type.Proposition where

data Proposition = TTrue
                 | FFalse
                 | Atom  Char
                 | And   Proposition Proposition
                 | Or    Proposition Proposition
                 | Not   Proposition
                 | Imply Proposition Proposition

instance Eq Proposition where
  (==) TTrue TTrue                 = True
  (==) FFalse FFalse               = False
  (==) (Atom a) (Atom b)           = a == b
  (==) (And a1 b1) (And a2 b2)     = (a1 == a2 && b1 == b2) || (a1 == b2 || a2 == b1)  
  (==) (Or a1 b1) (Or a2 b2)       = (a1 == a2 && b1 == b2) || (a1 == b2 || a2 == b1)
  (==) (Not a) (Not b)             = a == b
  (==) (Imply a1 b1) (Imply a2 b2) = a1 == a2 && b1 == b2
  (==) _ _                         = False

instance Show Proposition where
    show TTrue = "tt"
    show FFalse = "⊥"
    show (Atom c) = show c
    show (Not p) =  "¬" ++ showBracketIfNecessary p
    show (And p1 p2) = showBracketIfNecessary p1 ++ " & "  ++ showBracketIfNecessary p2
    show (Or p1 p2) = showBracketIfNecessary p1 ++ " ∨ "  ++ showBracketIfNecessary p2
    show (Imply p1 p2) = show p1 ++ " → " ++ show p2

andAll :: [Proposition] -> Proposition
andAll [] = TTrue
andAll [x] = x     
andAll (x:xs) = And x (andAll xs)

orAll :: [Proposition] -> Proposition
orAll []     = FFalse
orAll [x] = x     
orAll (x:xs) = Or x (andAll xs)

showBracketIfNecessary :: Proposition -> String
showBracketIfNecessary (Atom id) = show id
showBracketIfNecessary TTrue = show TTrue
showBracketIfNecessary FFalse = show FFalse
showBracketIfNecessary p = "(" ++ show p ++ ")"