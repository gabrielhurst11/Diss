module Propositional
    ( Prop(..)
    , Subst
    , Assoc
    , find
    , rmdups
    , eval
    , vars
    , bools
    , substs
    , isTaut
    , createTruthTable
    ) where

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop
          | BiImply Prop Prop
          deriving (Eq)

-- Define a Show instance for Prop
instance Show Prop where
    show (Const b) = show b
    show (Var x) = [x]
    show (Not p) = "Not ( " ++ show p ++ " )"
    show (And p q) = "( " ++ show p ++ " And " ++ show q ++ " ) "
    show (Imply a b) = "( " ++ show a ++ " Imply " ++ show b ++ " ) "
    show (Or a b) = " ( " ++ show a ++ " Or " ++ show b ++ " ) "
    show (BiImply p q) = "( " ++ show p ++ " BiImply " ++ show q ++ " ) "

-- Need to know value of variables - associates variables to logical values
type Subst = Assoc Char Bool

type Assoc a b = [(a, b)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k ==k']

-- removes Duplicates
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x: filter (/= x) (rmdups xs)

-- Function for evaluating prop given sub for variables on 5 possible forms
eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Or p q)    = (eval s p) || (eval s q)
eval s (BiImply p q)  = eval s p == eval s q

-- Consider all possible substitutions for variables a proposition contains
vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q)    = vars p ++ vars q
vars (BiImply p q) = vars p ++ vars q

-- Return all possible lists of logical values
-- Take 2 copies of lists produced, place False in one and True the other
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
          where bss = bools (n-1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where vs = rmdups (vars p)


-- Check if ALL values return true - tautology
isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

-- Maps Truth Values of Proposition Into String
-- Formatted so frontend can Display.
createTruthTable :: Prop -> String
createTruthTable prop = unlines $
  header : map formatTableRow table
  where
    variables = rmdups (vars prop)
    header =  unwords (map (:[]) variables) ++ "\t Result"
    substitutions = substs prop
    table = [(map (\var -> if find var s then 'T' else 'F') variables, eval s prop) | s <- substitutions]
    formatTableRow (vars', result) = unwords (map (:[]) vars' ++ if result then ["T"] else ["F"])