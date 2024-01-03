data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop
          deriving (Eq)

-- Define a Show instance for Prop
instance Show Prop where
    show (Const b) = show b
    show (Var x) = [x]
    show (Not p) = "Not (" ++ show p ++ ")"
    show (And p q) = "(" ++ show p ++ " And " ++ show q ++ ")"
    show (Imply a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
    show (Or a b) = "(" ++ show a ++ " Or " ++ show b ++ ")"

-- Need to know value of variables - associates variables to logical values
type Subst = Assoc Char Bool

type Assoc a b = [(a, b)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k ==k']

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

-- Consider all possible substitutions for variables a proposition contains
vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q)    = vars p ++ vars q

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

createTruthTable :: Prop -> IO ()
createTruthTable prop = do
  let variables = rmdups (vars prop)
  let header =  variables ++ "\t" ++ "Result"
  let substitutions = substs prop
  let table = [(map (\var -> if find var s then 'T' else 'F') variables, eval s prop) | s <- substitutions]
  
  putStrLn header
  mapM_ (\(vars', result) -> putStrLn (vars' ++ "\t" ++ if result then "T" else "F")) table

--Checks if any possible values return true (Satisfiable)
isSatisfiable :: Prop -> Bool
isSatisfiable p = or [eval s p | s <- substs p]

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

-- Returns conjunction if 2 values are known
conjInt :: Prop -> Prop -> Prop
conjInt x y = And x y

conjCheck :: Prop
conjCheck = And (Var 'A') (Var 'B')

-- Removes conjunction Left 
conjElimL :: Prop -> Prop
conjElimL (And p q) = p 

-- Removes conjunction Right
conjElimR :: Prop -> Prop
conjElimR (And p q) = q

impCheck1 :: Prop
impCheck1 = Imply (Var 'A') (Var 'B')

impCheck2 :: Prop
impCheck2 = (Var 'A')

-- If implication known q returned
impElim :: Prop -> Prop -> Maybe Prop
impElim (Imply p q) (y)
    | p == y = Just q
    | otherwise = Nothing

disjTest :: Prop
disjTest = Var 'A'

-- Introduces disjunction on the left
disjIntL :: Prop -> Prop
disjIntL p = Or p q where q = (Var 'Z') 

-- Introduces disjunction on the right
disjIntR :: Prop -> Prop
disjIntR p = Or q p where q = (Var 'Z')

   
-- Find a way to implement a -> b [x...a] / a
-- Natural deduction (Represented as a tree?)
data Tree a = Leaf a | Node (Tree a) (Tree a)

-- Example Tree
exampleTree :: Tree Prop
exampleTree = Node (Node (Leaf p1) (Leaf p2)) (Leaf p3)

-- A and Not A
p1 :: Prop 
p1 = And (Var 'A')(Not(Var 'A'))

-- A and B implies A
p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

-- A implies A and B
p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

-- (A and A implies B) implies B
p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')


-- (A and B implies B and C)
p5 :: Prop
p5 = Imply (And (Var 'A') (Var 'B')) (And (Var 'B') (Var 'C'))

-- A or B
p6 :: Prop
p6 = Or (Var 'A') (Var 'B')