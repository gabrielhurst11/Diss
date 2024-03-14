module Functions
    ( condition
    , antecedent
    , consequent
    , modusPonens
    , negation
    , conjInt
    , conjElimL
    , conjElimR
    , impElim
    , disjIntL
    , disjIntR
    ) where

import Propositional (Prop(..))

-- Set up Prop to check Modus Ponens can be applied
-- A implies B
-- A and B also defined
condition :: Prop
condition = Imply (Var 'A') (Var 'B')

antecedent :: Prop
antecedent = Var 'A'

consequent :: Prop
consequent = Var 'B'

-- Define a function to apply Modus Ponens
modusPonens :: Prop -> Prop -> Prop -> Maybe Prop
modusPonens con ant csq
    | con == Imply ant csq = Just csq
    | otherwise = Nothing

-- Negation function
negation :: Prop -> Prop
negation p = Not p

-- Returns conjunction if 2 values are known
conjInt :: Prop -> Prop -> Prop
conjInt x y = And x y

-- Removes conjunction Left 
conjElimL :: Prop -> Maybe Prop
conjElimL (And p q) =Just p 
conjElimL _ = Nothing

-- Removes conjunction Right
conjElimR :: Prop -> Maybe Prop
conjElimR (And p q) = Just q
conjElimR _ = Nothing

-- If implication known q returned
impElim :: Prop -> Prop -> Maybe Prop
impElim (Imply p q) y
    | p == y = Just q
    | otherwise = Nothing
impElim _ _ = Nothing

-- Introduces disjunction on the left
disjIntL :: Prop -> Prop
disjIntL p = Or p q where q = Var 'Z'

-- Introduces disjunction on the right
disjIntR :: Prop -> Prop
disjIntR p = Or q p where q = Var 'Z'