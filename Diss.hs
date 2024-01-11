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

negation :: Prop -> Prop
negation p = Not p

-- Returns conjunction if 2 values are known
conjInt :: Prop -> Prop -> Prop
conjInt x y = And x y

-- Removes conjunction Left 
conjElimL :: Prop -> Prop
conjElimL (And p q) = p 

-- Removes conjunction Right
conjElimR :: Prop -> Prop
conjElimR (And p q) = q


-- If implication known q returned
impElim :: Prop -> Prop -> Maybe Prop
impElim (Imply p q) (y)
    | p == y = Just q
    | otherwise = Nothing


-- Introduces disjunction on the left
disjIntL :: Prop -> Prop
disjIntL p = Or p q where q = (Var 'Z') 

-- Introduces disjunction on the right
disjIntR :: Prop -> Prop
disjIntR p = Or q p where q = (Var 'Z')

   
-- Find a way to implement a -> b [x...a] / a
-- Natural deduction (Represented as a tree?)
data Tree a = Leaf a | Node (Tree a) (Tree a)

-- Natural Deduction Tree
naturalDedTree :: Tree Prop
naturalDedTree = Node (Node (Leaf p1) (Leaf p2)) (Leaf p3)

data NDTree = Assumption Prop
            | Rule String [NDTree] Prop

exampleProofTree :: NDTree
exampleProofTree = Rule "Final Rule" [stepB, stepC] (Var 'l')
  where
    stepB = Rule "Some Rule" [Assumption (Var 'a')] (Var 'b')
    stepC = Rule "Another Rule" [Assumption (Var 'b')] (Var 'c')

-- Function to convert NDTree to a string representation with indentation
printNDTree :: NDTree -> IO ()
printNDTree tree = printNDTreeHelper 0 tree
  where
    printNDTreeHelper indent (Assumption p) = putStrLn $ replicate indent ' ' ++ "Assumption: " ++ show p
    printNDTreeHelper indent (Rule ruleName premises conclusion) = do
        putStrLn $ replicate indent ' ' ++ "Rule: " ++ ruleName
        mapM_ (printNDTreeHelper (indent + 2)) premises
        putStrLn $ replicate indent ' ' ++ "Conclusion: " ++ show conclusion


myTree :: NDTree
myTree = Rule "Implication Elimination" [Assumption (Var 'A'), Rule "Modus Ponens" [Assumption (Imply (Var 'A') (Var 'B'))] (Var 'B')] (Var 'B')

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

conjCheck :: Prop
conjCheck = And (Var 'A') (Var 'B')

impCheck1 :: Prop
impCheck1 = Imply (Var 'A') (Var 'B')

impCheck2 :: Prop
impCheck2 = (Var 'A')

disjTest :: Prop
disjTest = Var 'A'

-- Main function
main :: IO ()
main = do
    let a = Var 'A'
    let b = Var 'B'
    let c = Var 'C'
    let aImpB = Imply a b
    let bImpC = Imply b c
    let aImpC = Imply a c

    let premise1 = Assumption aImpB
    let premise2 = Assumption bImpC
    let premise3 = Assumption a

    let step1 = Rule "Modus Ponens on A -> B and A" [premise1, premise3] b
    let step2 = Rule "Modus Ponens on B -> C and B" [premise2, step1] c
    let conclusion = Rule "Hypothetical Syllogism from A -> B, B -> C to A -> C" [premise1, premise2, step2] aImpC

    printNDTree conclusion


proofExample :: IO ()
proofExample = do
    let p = Var 'P'
    let q = Var 'Q'
    let pAndQ = And p q
    let qAndP = And q p
    let pAndQImpQAndP = Imply pAndQ qAndP
    let premise1 = Assumption pAndQ
    let premise2 = Assumption p
    let premise3 = Assumption q

    let step1 = Rule "Conjunction Elimination on P AND Q" [premise1] p
    let step2 = Rule "Conjunction Elimination on P AND Q" [premise1] q
    let step3 = Rule "Conjunction Introduction on P, Q" [premise2, premise3] qAndP
    let conclusion = Rule "Implication Introduction on P AND Q, Q AND P" [premise1, step3] pAndQImpQAndP
    printNDTree conclusion


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


convertCNF :: Prop -> Prop
convertCNF p = distribute(pushNegation (elimImp(p)))

pTest :: Prop
pTest = Imply (And (Var 'P') (Var 'Q')) (And (Var 'Q') (Var 'R'))