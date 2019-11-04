module Type.Sequent where

import Type.Proposition
import Data.List

data Sequent = Sequent [Proposition] [Proposition]
             deriving (Eq)

instance Show Sequent where
    show (Sequent [] []) = "⊢"
    show (Sequent [] p2) = "⊢ " ++ show p2
    show (Sequent p1 []) = show (reverse p1) ++ " ⊢"
    show (Sequent p1 p2) = show (reverse p1) ++ " ⊢ " ++ show p2
    
opposite :: Sequent -> Sequent
opposite (Sequent sx dx) = Sequent [] [Not $ Imply (andAll sx) (orAll dx)]