module Type.Sequent where

import Type.Proposition

data Sequent = Sequent [Proposition] [Proposition]
             deriving (Eq)

instance Show Sequent where
    show (Sequent [] []) = "⊢"
    show (Sequent [] p2) = "⊢ " ++ show p2
    show (Sequent p1 []) = show p1 ++ " ⊢"
    show (Sequent p1 p2) = show p1 ++ " ⊢ " ++ show p2