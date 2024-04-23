module Cnf
    ( Clause
    , ClauseSet
    , cnfConversionSteps
    , elimImp
    , pushNegation
    , distribute
    , findUnitClause
    , getClauseSet
    , findCNFString
    , dpllTree
    , pickLiteral
    , removeTautClauses
    , getFirstLiteral
    ) where

import Propositional (Prop(..))
import Functions (negation)
import Data.List (intercalate)

type Clause = [Prop]
type ClauseSet = [Clause]

-- Eliminate Implications, p -> q becomes -p \/ q 
elimImp :: Prop -> Prop
elimImp (Imply p q) = Or (Not (elimImp p)) (elimImp q)
elimImp (BiImply p q) = And  (elimImp (Imply p q)) (elimImp(Imply q p))
elimImp (And p q) = And (elimImp p) (elimImp q)
elimImp (Or p q) = Or (elimImp p) (elimImp q)
elimImp (Not p) = Not (elimImp p)
elimImp p = p

-- Push negations inwards
pushNegation :: Prop -> Prop
pushNegation (Not (Not p))  = pushNegation p
pushNegation (Not (And p q)) = Or (pushNegation (Not p)) (pushNegation (Not q))
pushNegation (Not (Or p q))  = And (pushNegation (Not p)) (pushNegation (Not q))
pushNegation (And p q)      = And (pushNegation p) (pushNegation q)
pushNegation (Or p q)       = Or (pushNegation p) (pushNegation q)
pushNegation p              = p

-- Distribute Disjunctions over Conjunctions
distribute :: Prop -> Prop
distribute (Or p (And q r)) = And (distribute (Or p q)) (distribute (Or p r))
distribute (Or (And p q) r) = And (distribute (Or p r)) (distribute (Or q r))
distribute (And p q)        = And (distribute p) (distribute q)
distribute (Or p q)         = Or (distribute p) (distribute q)
distribute p                = p 

-- Generate clause sets from Propositions
getClauseSet :: Prop -> ClauseSet
getClauseSet (Var p) = [[Var p]]
getClauseSet (Not (Var p)) = [[Not (Var p)]]
getClauseSet (And p q) = getClauseSet p ++ getClauseSet q
getClauseSet (Or p q) = [pClause ++ qClause | pClause <- getClauseSet p, qClause <- getClauseSet q]
getClauseSet _ = []

-- Removes clauses which are always True
-- Checks if any clause contains literal 'l' and also 'not l'
removeTautClauses :: ClauseSet -> ClauseSet
removeTautClauses = filter (not . isTautologicalClause)
  where
    isTautologicalClause :: Clause -> Bool
    isTautologicalClause clause = any (\p -> Not p `elem` clause) clause


-- Shows ClaueSet datatype as String
findCNFString :: ClauseSet -> String
findCNFString [] = "{}"
findCNFString clauses = "{ " ++ intercalate " , " (map showClause clauses) ++ " } "

-- Shows Clause datatype as a String
showClause :: Clause -> String
showClause [] = "{}"
showClause props = "{ " ++ intercalate ", " (map showProp props) ++ " } "

-- Shows Propositions as a String
showProp :: Prop -> String
showProp (Var v) = [v]
showProp (Not p) = "Not " ++ showProp p
showProp p = show p


-- Applies CNF Conversion steps and generates DPLL tree
-- Creates String containing all steps and information
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


-- Checks if clause is Unit Clause
unitClauseCheck :: Clause -> Bool
unitClauseCheck [l] = True
unitClauseCheck _ = False

--Removes Clauses containing entered Proposition
removeClauseLiterals :: Prop -> ClauseSet -> ClauseSet
removeClauseLiterals l clauses = filter (\clause -> l `notElem` clause) clauses

-- Removes Negation of entered Proposition from clauses
removeNegatedProp :: Prop -> ClauseSet -> ClauseSet
removeNegatedProp p clauses = map (filter (/= negation p)) clauses

-- Extracts literal 'l' from clause set - case split on positive value
extractProp :: Clause -> Prop
extractProp [(Not l)] = l
extractProp [l] = l
extractProp _ = Const False

-- Find if ClauseSet has Unit Clause and returns Prop if true 
findUnitClause :: ClauseSet -> Maybe Prop
findUnitClause [] = Nothing
findUnitClause (clause : rest)
    | unitClauseCheck clause = Just $ extractProp clause
    | otherwise = findUnitClause rest


-- Returns first literal of clause in ClauseSet
pickLiteral :: ClauseSet -> Maybe Prop
pickLiteral [] = Nothing
pickLiteral (clause : _) = getFirstLiteral clause

-- Extracts first literal from a clause
getFirstLiteral :: Clause -> Maybe Prop
getFirstLiteral [] = Nothing 
getFirstLiteral ((Not prop): _) = Just prop
getFirstLiteral (prop : _) = Just prop


-- Returns ClauseSet after Clauses containing literal are deleted
-- and negated literal removed from clauses
applyStep2functions :: Prop -> ClauseSet -> ClauseSet
applyStep2functions p clauses = removeNegatedProp p (removeClauseLiterals p clauses)

-- Applies DPLL steps but checks for empty Clauses and ClauseSets
applyDPLLStep :: Prop -> ClauseSet -> ClauseSet
applyDPLLStep _ [] = []
applyDPLLStep _ [[]] = [[]]
applyDPLLStep p clauses = applyStep2functions (p) (clauses)


-- Each Node either a Bool (SAT or UNSAT) 
-- Or parent Node with CaseSplit Prop and Current ClauseSet
data DPLLTree = Leaf Bool | Node Prop ClauseSet DPLLTree DPLLTree

-- Show Instance for DPLL Tree - In format to be parsed by Frontend
instance Show DPLLTree where
    show tree = formatTree tree
        where
            formatTree (Leaf True) = "SAT"
            formatTree (Leaf False) = "UNSAT"
            formatTree (Node p clauses left right) =
                let leftStr = formatTree left
                    rightStr = formatTree right
                in show p ++ " ( " ++ leftStr ++ " ) ( " ++ rightStr ++ " )"

                
-- Recursively Builds DPLLTree
dpllTree :: ClauseSet -> DPLLTree
dpllTree clauses
    | null clauses = Leaf True  -- SAT
    | any null clauses = Leaf False  -- UNSAT
    | otherwise =
        --Checks for Unit clause
        case findUnitClause clauses of
            Just unitProp ->
                -- CaseSplit on Unit Clause
                let applyTrue = (applyDPLLStep (unitProp) clauses)
                    applyFalse = (applyDPLLStep (negation unitProp) clauses)
                in Node unitProp (clauses) (dpllTree (applyTrue))  (dpllTree (applyFalse)) 
            Nothing ->
                -- Picks first literal if no Unit CLause
                case pickLiteral clauses of
                    Just caseSplitVar ->
                        let applyTrue = (applyDPLLStep (caseSplitVar) clauses) 
                            applyFalse = (applyDPLLStep (negation caseSplitVar) clauses)
                        in Node caseSplitVar (clauses) (dpllTree (applyTrue)) (dpllTree (applyFalse))
                    Nothing -> Leaf False
