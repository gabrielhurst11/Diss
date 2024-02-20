module Cnf
    ( Clause
    , ClauseSet
    , elimImp
    , pushNegation
    , distribute
    , containsContradiction
    , filterContradictions
    , convertToClauses
    , convertToClause
    , convertCNF
    , pTest
    , convertAndFilterCNF
    ) where

import Propositional (Prop(..))

type Clause = [Prop]
type ClauseSet = [Clause]

elimImp :: Prop -> Prop
elimImp (Imply p q) = Or (Not (elimImp p)) (elimImp q)
elimImp (And p q) = And (elimImp p) (elimImp q)
elimImp (Or p q) = Or (elimImp p) (elimImp q)
elimImp (Not p) = Not (elimImp p)
elimImp p = p

pushNegation :: Prop -> Prop
pushNegation (Not (Not p))  = pushNegation p
pushNegation (Not (And p q)) = Or (pushNegation (Not p)) (pushNegation (Not q))
pushNegation (Not (Or p q))  = And (pushNegation (Not p)) (pushNegation (Not q))
pushNegation (And p q)      = And (pushNegation p) (pushNegation q)
pushNegation (Or p q)       = Or (pushNegation p) (pushNegation q)
pushNegation p              = p

distribute :: Prop -> Prop
distribute (Or p (And q r)) = And (distribute (Or p q)) (distribute (Or p r))
distribute (Or (And p q) r) = And (distribute (Or p r)) (distribute (Or q r))
distribute (And p q)        = And (distribute p) (distribute q)
distribute (Or p q)         = Or (distribute p) (distribute q)
distribute p                = p 

-- Function to check if a clause contains contradictory propositions
containsContradiction :: Clause -> Bool
containsContradiction clause = any (isNegationPresent clause) clause

-- Check if the negation of a proposition is present in the clause
isNegationPresent :: Clause -> Prop -> Bool
isNegationPresent clause prop = case prop of
    Not p -> p `elem` clause
    _     -> Not prop `elem` clause

-- Function to filter out clauses with contradictions
filterContradictions :: ClauseSet -> ClauseSet
filterContradictions = filter (not . containsContradiction)

convertToClauses :: Prop -> ClauseSet
convertToClauses (And p q) = convertToClauses p ++ convertToClauses q
convertToClauses p         = [convertToClause p]

convertToClause :: Prop -> Clause
convertToClause (Or p q) = convertToClause p ++ convertToClause q
convertToClause p        = [p]

convertCNF :: Prop -> Prop
convertCNF p = distribute(pushNegation (elimImp(p)))

pTest :: Prop
pTest = Imply (Or (Var 'P') (Var 'Q')) (Or (Var 'Q') (Var 'R'))

convertAndFilterCNF :: Prop -> ClauseSet
convertAndFilterCNF p = filterContradictions(convertToClauses(convertCNF p))