module Functions
    ( modusPonens
    , negation
    , conjInt
    , conjElimL
    , conjElimR
    , disjIntL
    , disjIntR
    , impInt
    , deMorganLaw
    , modusTolens
    ) where

import Propositional (Prop(..))


-- Implication Elimination (Modus Ponens)
modusPonens :: Prop -> Prop -> Maybe Prop
modusPonens (Imply p q) r
    | p == r = Just q
    | otherwise = Nothing
modusPonens _ _ = Nothing

modusTolens :: Prop -> Prop -> Maybe Prop
modusTolens (Imply p q) r
    | negation q == r = Just (negation p)
    | otherwise = Nothing
modusTolens _ _ = Nothing

    
-- Negation function
negation :: Prop -> Prop
negation (Not p) = p
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
deMorganLaw (BiImply p q) = do
    p' <- deMorganLaw p
    q' <- deMorganLaw q
    return (BiImply p' q')
deMorganLaw (Var q) = Just (Var q)
deMorganLaw (Const p) = Just (Const p)