module Cnf
    ( Clause
    , ClauseSet
    , elimImp
    , pushNegation
    , distribute
    , pTest
    , pTest2
    , pTest3
    , pTest4
    , getClauseSet
    , findCNF
    , removeTautClauses
    , removeClauseLiterals
    , removeNegatedProp
    ) where

import Propositional (Prop(..))
import Functions (negation)

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

getClauseSet :: Prop -> ClauseSet
getClauseSet (Var p) = [[Var p]]
getClauseSet (Not (Var p)) = [[Not (Var p)]]
getClauseSet (And p q) = getClauseSet p ++ getClauseSet q
getClauseSet (Or p q) = [pClause ++ qClause | pClause <- getClauseSet p, qClause <- getClauseSet q]

findCNF :: Prop -> ClauseSet
findCNF p = getClauseSet(distribute(pushNegation(elimImp(p))))

removeTautClauses :: ClauseSet -> ClauseSet
removeTautClauses = filter (not . isTautologicalClause)
  where
    isTautologicalClause :: Clause -> Bool
    isTautologicalClause clause = any (\p -> Not p `elem` clause) clause

unitClauseCheck :: Clause -> Bool
unitClauseCheck [l] = True
unitClauseCheck _ = False

removeClauseLiterals :: Prop -> ClauseSet -> ClauseSet
removeClauseLiterals l clauses = filter (\clause -> l `notElem` clause) clauses

-- Remove the negation of a proposition from clauses containing it
removeNegatedProp :: Prop -> ClauseSet -> ClauseSet
removeNegatedProp p clauses = map (filter (/= negation p)) clauses


pTest :: Prop
pTest = Imply (Or (Var 'P') (Var 'Q')) (Or (Var 'Q') (Var 'R'))

pTest2 :: Prop
pTest2 = Imply (Imply (Imply (Var 'P') (Var 'Q')) (Var 'P')) (Var 'P')

pTest3 :: Prop
pTest3 = Not(Imply (And (Var 'P') (Var 'Q')) (And (Var 'Q') (Var 'R')))

pTest4 :: ClauseSet
pTest4 = [[(Var 'A')], [(Var 'A'), (Var 'B')], [(Var 'C'), (Not (Var 'A'))] ]

