module Cnf
    ( Clause
    , ClauseSet
    , cnfConversionSteps
    , elimImp
    , pushNegation
    , distribute
    , findCNF
    , findUnitClause
    , getClauseSet
    , findCNFString
    , dpllTree
    , clauses1
    , pickLiteral
    , testProp
    , removeTautClauses
    , getFirstLiteral
    ) where

import Propositional (Prop(..))
import Functions (negation)
import Data.List (intercalate)

type Clause = [Prop]
type ClauseSet = [Clause]

elimImp :: Prop -> Prop
elimImp (Imply p q) = Or (Not (elimImp p)) (elimImp q)
elimImp (BiImply p q) = And  (elimImp (Imply p q)) (elimImp(Imply q p))
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
getClauseSet _ = []


findCNF :: Prop -> ClauseSet
findCNF p = removeTautClauses(getClauseSet(distribute(pushNegation(elimImp(p)))))

findCNFString :: ClauseSet -> String
findCNFString [] = "{}"
findCNFString clauses = "{ " ++ intercalate " , " (map showClause clauses) ++ " } "

showClause :: Clause -> String
showClause [] = "{}"
showClause props = "{ " ++ intercalate ", " (map showProp props) ++ " } "

showProp :: Prop -> String
showProp (Var v) = [v]
showProp (Not p) = "Not " ++ showProp p
showProp p = show p


cnfConversionSteps :: Prop -> String
cnfConversionSteps prop =
    let step1 = elimImp prop
        step2 = pushNegation step1
        step3 = distribute step2
        step4 = getClauseSet step3
        step5 = removeTautClauses step4
        dplltree = dpllTree step5
    in unlines $
            [ "Step 1: (Eliminate Implications): " ++ show step1
            , "Step 2: (Push Negations): " ++ show step2
            , "Step 3: (Distribute): " ++ show step3
            , "Step 4: (Get Clause Sets): " ++ findCNFString step4 
            , "Step 5: (Remove Tautological Clauses): " ++ findCNFString step5
            , show dplltree
            ]

testProp :: Prop
testProp = Or (And (Var 'P') (Var 'Q')) (And (Var 'Q') (Var 'P'))


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


removeNegatedProp :: Prop -> ClauseSet -> ClauseSet
removeNegatedProp p clauses = map (filter (/= negation p)) clauses

extractProp :: Clause -> Prop
extractProp [(Not l)] = l
extractProp [l] = l

findUnitClause :: ClauseSet -> Maybe Prop
findUnitClause [] = Nothing
findUnitClause (clause : rest)
    | unitClauseCheck clause = Just $ extractProp clause
    | otherwise = findUnitClause rest


pickLiteral :: ClauseSet -> Maybe Prop
pickLiteral [] = Nothing
pickLiteral (clause : _) = getFirstLiteral clause


getFirstLiteral :: Clause -> Maybe Prop
getFirstLiteral [] = Nothing 
getFirstLiteral ((Not prop): _) = Just prop
getFirstLiteral (prop : _) = Just prop


applyStep2functions :: Prop -> ClauseSet -> ClauseSet
applyStep2functions p clauses = removeNegatedProp p (removeClauseLiterals p clauses)


clauses :: ClauseSet
clauses = [[(Var 'P'),(Var 'Q')],[(Not (Var 'P')),(Var 'R')]]

applyDPLLStep :: Prop -> ClauseSet -> ClauseSet
applyDPLLStep _ [] = []
applyDPLLStep _ [[]] = [[]]
applyDPLLStep p clauses = applyStep2functions (p) (clauses)


data DPLLTree = Leaf Bool | Node Prop ClauseSet DPLLTree DPLLTree

instance Show DPLLTree where
    show tree = formatTree tree
        where
            formatTree (Leaf True) = "SAT"
            formatTree (Leaf False) = "UNSAT"
            formatTree (Node p clauses left right) =
                let leftStr = formatTree left
                    rightStr = formatTree right
                in show p ++ " ( " ++ leftStr ++ " ) ( " ++ rightStr ++ " )"

                
dpllTree :: ClauseSet -> DPLLTree
dpllTree clauses
    | null clauses = Leaf True  -- SAT
    | any null clauses = Leaf False  -- UNSAT
    | otherwise =
        case findUnitClause clauses of
            Just unitProp ->
                let applyTrue = (applyDPLLStep (unitProp) clauses)
                    applyFalse = (applyDPLLStep (negation unitProp) clauses)
                in Node unitProp (clauses) (dpllTree (applyTrue))  (dpllTree (applyFalse)) 
            Nothing ->
                case pickLiteral clauses of
                    Just caseSplitVar ->
                        let applyTrue = (applyDPLLStep (caseSplitVar) clauses) 
                            applyFalse = (applyDPLLStep (negation caseSplitVar) clauses)
                        in Node caseSplitVar (clauses) (dpllTree (applyTrue)) (dpllTree (applyFalse))
                    Nothing -> Leaf False


clauses1 :: ClauseSet
clauses1 = [[(Not(Var 'A'))],[(Not (Var 'A')),(Var 'B'),(Var 'C')],[(Var 'A'),(Var 'C'),(Not(Var 'D'))], [(Var 'A'),(Not(Var 'C')),(Var 'D')],[(Var 'A'),Not((Var 'C')),(Not(Var 'D'))]
            , [(Not(Var 'B')),(Not(Var 'C')),(Var 'D')], [(Not(Var 'A')),(Var 'B'),(Not(Var 'C'))],[(Not(Var 'A')),(Not(Var 'B')),(Var 'C')]]