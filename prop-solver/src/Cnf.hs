module Cnf
    ( Clause
    , ClauseSet
    , cnfConversionSteps
    , elimImp
    , pushNegation
    , distribute
    , getClauseSet
    ) where

import Propositional (Prop(..))
import Functions (negation)
import Data.List (intercalate)

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
getClauseSet _ = []


findCNF :: Prop -> ClauseSet
findCNF p = removeTautClauses(getClauseSet(distribute(pushNegation(elimImp(p)))))

findCNFString :: ClauseSet -> String
findCNFString clauses = intercalate " , " (map showClause clauses)

showClause :: Clause -> String
showClause [] = "False"
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
        step6 = allUnitClauses step5
        value = checkSAT step6
    in case value of
        Just True -> unlines $
            [ "Step 1 (Eliminate Implications): " ++ show step1
            , "Step 2 (Push Negations): " ++ show step2
            , "Step 3 (Distribute): " ++ show step3
            , "Step 4 (Get Clause Sets): " ++ findCNFString step4 
            , "Step 5 (Remove Tautological Clauses): " ++ findCNFString step5
            , "DPLL Algorithm"
            , "Step 1 For each Unit Clause 'l': "
            , "Delete Clauses containing l"
            , "Delete Not l from all clauses"
            , "Final for each Unit Clause" ++ findCNFString step6
            , "The Proposition is Satisfiable as it has returned an empty clause set"
            ]
        Just False -> unlines $
            [ "Step 1 (Eliminate Implications): " ++ show step1
            , "Step 2 (Push Negations): " ++ show step2
            , "Step 3 (Distribute): " ++ show step3
            , "Step 4 (Get Clause Sets): " ++ findCNFString step4 
            , "Step 5 (Remove Tautological Clauses): " ++ findCNFString step5
            , "DPLL Algorithm"
            , "Step 1 For each Unit Clause 'l': "
            , "Delete Clauses containing l"
            , "Delete Not l from all clauses"
            , "Final for each Unit Clause" ++ findCNFString step6
            , "The Proposition is UnSatisfiable as it has returned an empty clause within the Clause Set"
            ]
        Nothing -> 
            let step7 = applyStep5 step6
                newValue = checkSATSplit <$> step7
            in case newValue of 
                Just True -> unlines $
                    [ "Step 1 (Eliminate Implications): " ++ show step1
                    , "Step 2 (Push Negations): " ++ show step2
                    , "Step 3 (Distribute): " ++ show step3
                    , "Step 4 (Get Clause Sets): " ++ findCNFString step4 
                    , "Step 5 (Remove Tautological Clauses): " ++ findCNFString step5
                    , "DPLL Algorithm"
                    , "Step 1 For each Unit Clause 'l': "
                    , "Delete Clauses containing l"
                    , "Delete Not l from all clauses"
                    , "Final for each Unit Clause" ++ findCNFString step6
                    , "There are no more unit clauses to check, so pick literal and perform case split"
                    , "Proposition is Satisfiable as case split returned [],[]"
                    ]
                Just False -> unlines $
                    [ "Step 1 (Eliminate Implications): " ++ show step1
                    , "Step 2 (Push Negations): " ++ show step2
                    , "Step 3 (Distribute): " ++ show step3
                    , "Step 4 (Get Clause Sets): " ++ findCNFString step4 
                    , "Step 5 (Remove Tautological Clauses): " ++ findCNFString step5
                    , "DPLL Algorithm"
                    , "Step 1 For each Unit Clause 'l': "
                    , "Delete Clauses containing l"
                    , "Delete Not l from all clauses"
                    , "Final for each Unit Clause" ++ findCNFString step6
                    , "There are no more unit clauses to check, so pick literal and perform case split"
                    , "Proposition is Unsatisfiable as case split returned an empty clause in the clauseSet [],[]"
                    ]
                Nothing -> "Satisfiability couldn't be determined."

         
finalDPLL :: Prop -> Maybe Bool
finalDPLL p = case checkSAT dpllStep of
    Just True -> Just True
    Just False -> Just False
    Nothing -> checkStep5
  where
    cnfConv = findCNF p
    dpllStep = allUnitClauses cnfConv
    checkStep5 = checkSATSplit <$> (applyStep5 dpllStep)


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

applyStep2functions :: Prop -> ClauseSet -> ClauseSet
applyStep2functions p clauses = removeNegatedProp p (removeClauseLiterals p clauses)

extractProp :: Clause -> Prop
extractProp [l] = l

findUnitClause :: ClauseSet -> Maybe Prop
findUnitClause [] = Nothing
findUnitClause (clause : rest)
    | unitClauseCheck clause = Just $ extractProp clause
    | otherwise = findUnitClause rest

applyStep2 :: ClauseSet -> ClauseSet
applyStep2 clauses =
    case findUnitClause clauses of
        Just p -> applyStep2functions p clauses
        Nothing -> clauses

allUnitClauses :: ClauseSet -> ClauseSet
allUnitClauses clauses
    | (applyStep2 clauses) == clauses = clauses
    | otherwise = allUnitClauses (applyStep2 clauses)

applyStep5 :: ClauseSet -> Maybe [ClauseSet]
applyStep5 clauses =
    case pickLiteral clauses of
        Just p -> do
            let clausesAddT = addClause clauses p
            let applyTrue = (allUnitClauses clausesAddT) 
            let clausesAddF = addClause clauses (Not p)
            let applyFalse = (allUnitClauses clausesAddF)
            Just [applyTrue, applyFalse]
        otherwise -> Nothing


addClause :: ClauseSet -> Prop -> ClauseSet
addClause [] p = [[p]]
addClause clauses p = [p] : clauses


pickLiteral :: ClauseSet -> Maybe Prop
pickLiteral [] = Nothing
pickLiteral (clause : _) = getFirstLiteral clause

-- Define a function to get the first proposition from a clause
getFirstLiteral :: Clause -> Maybe Prop
getFirstLiteral [] = Nothing 
getFirstLiteral (prop : _) = Just prop

checkSAT :: ClauseSet -> Maybe Bool
checkSAT [] = Just True
checkSAT [[]] = Just False
checkSAT _ = Nothing

checkSATSplit :: [ClauseSet] -> Bool
checkSATSplit [[],[]] = True
checkSATSplit _ = False 
