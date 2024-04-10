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
    , impInt
    , deMorganLaw
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
conjInt :: Prop -> Prop -> Maybe Prop
conjInt x y = Just (And x y)

-- Removes conjunction Left 
conjElimL :: Prop -> Maybe Prop
conjElimL (And p _) =Just p 
conjElimL _ = Nothing

-- Removes conjunction Right
conjElimR :: Prop -> Maybe Prop
conjElimR (And _ q) = Just q
conjElimR _ = Nothing

-- If implication known q returned
impElim :: Prop -> Prop -> Maybe Prop
impElim (Imply p q) y
    | p == y = Just q
    | otherwise = Nothing
impElim _ _ = Nothing

-- Introduces disjunction on the left
disjIntL :: Prop -> Prop -> Maybe Prop
disjIntL p q = Just (Or p q) 

-- Introduces disjunction on the right
disjIntR :: Prop -> Prop -> Maybe Prop
disjIntR p q = Just (Or q p)

impInt :: Prop -> Prop -> Maybe Prop
impInt p q = Just (Imply p q)

-- Apply De Morgan's Law to a given proposition, returning Maybe Prop
deMorganLaw :: Prop -> Maybe Prop
deMorganLaw (Not (And p q)) = do
    p1 <- deMorganLaw p
    q1 <- deMorganLaw q
    return (Or (Not p1) (Not q1))
deMorganLaw (Not (Or p q)) = do
    p2 <- deMorganLaw p
    q2 <- deMorganLaw q
    return (And (Not p2) (Not q2))
deMorganLaw (And p q) = do
    p' <- deMorganLaw p
    q' <- deMorganLaw q
    return (And p' q')
deMorganLaw (Or p q) = do
    p' <- deMorganLaw p
    q' <- deMorganLaw q
    return (Or p' q')
deMorganLaw (Not p) = do
    p' <- deMorganLaw p
    return (Not p')
deMorganLaw (Imply p q) = do
    p' <- deMorganLaw p
    q' <- deMorganLaw q
    return (Imply p' q')
deMorganLaw (Var q) = Just (Var q)
deMorganLaw (Const p) = Just (Const p)