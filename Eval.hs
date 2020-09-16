module Eval where
import Grammar
import LangTypes

-- First String is Variable Name, Second is value as string
--type Store = [(String,String)] 
type State = (Statement,[Statement],Store)

toState :: Statement -> [[Int]] -> State
toState x origList = (head $ encase x, tail $ encase x,[("origLists",show origList)])

encase x = [x]
seq (Sequence x1 x2) rest store = encase x1 ++ encase x2

----------------------------------------------------------------

eval :: State -> State 

-- IntAssignment  Int i = 3
eval ((IntAssignment var i), rest, store) 
    | typeOf store i == TInt && length rest > 0 && varTaken var store = eval (head rest, tail rest, pop (var, i) (replace (var, show $ evalExpr store i) store))
    | typeOf store i == TInt && length rest > 0 = eval (head rest, tail rest, pop (var, i) ((var, show $ evalExpr store i):store))
    | length rest <= 0 = error "Invalid End of Program"
    | typeOf store i /= TInt = error ("[Type Error] Expected: TInt, Actual: " ++ (show (typeOf store i)))

-- BoolAssignment Bool x = true
eval ((BoolAssignment var x), rest, store)
    | typeOf store x == TBool && length rest > 0 && varTaken var store = eval (head rest, tail rest, replace (var, show $ evalBool store x) store)
    | typeOf store x == TBool && length rest > 0 = eval (head rest, tail rest, (var, show $ evalBool store x):store)
    | length rest <= 0 = error "Invalid End of Program"
    | typeOf store x /= TBool = error ("[Type Error] Expected: TBool, Actual: " ++ (show (typeOf store x)))

-- ListAssignment List x = [1,2,3] 
eval ((ListAssignment var x) , rest, store)
     | typeOf store x == TList && length rest > 0 && varTaken var store = error "Duplicate variable"
     | typeOf store x == TList && length rest > 0 = eval (head rest, tail rest, (var, show $ evalL store x):store)
     | length rest <= 0 = error "Invalid End of Program"
     | typeOf store x /= TList = error ("[Type Error] Expected: TList, Actual: " ++ (show (typeOf store x)))

-- Matrix x = getLines
eval ((MatrixAssignment var), rest, store)
    | length rest > 0 && varTaken var store = error "Duplicate variable"
    | length rest > 0 = eval (head rest, tail rest, (var, origLists):store)
    | length rest <= 0 = error "Invalid End of Program" 
        where origLists = lookupVar "origLists" store
       
-- push
eval ((Push intVar listVar), rest, store)
    | length rest > 0 = eval (head rest, tail rest, push intVar listVar store)
    | length rest <= 0 = error "Invalid End of Program"

-- sequence
eval ((Sequence stmt1 stmt2), rest, store) = eval (stmt1, [stmt2] ++ rest, store)

-- while
eval ((While b stmt), rest, store) 
   | typeOf store (evalBool store b) == TBool && evalBool store b == CTrue = eval (stmt, (While b stmt) : rest, store)
   | typeOf store (evalBool store b) /= TBool = error ("[Type Error] Expected: Boolean, Actual: " ++ (show (typeOf store b)))
   | otherwise = eval (head rest, tail rest, store)

-- ifthenelse
eval ((IfThenElse expr stmt1 stm2), rest, store)
   | typeOf store (evalBool store expr) == TBool && evalBool store expr == CTrue = eval (stmt1, rest, store)
   | typeOf store (evalBool store expr) /= TBool = error ("[Type Error] Expected: Boolean, Actual: " ++ (show (typeOf store expr)))
   | otherwise = eval (stm2, rest, store) 

-- return
eval ((Return var), rest, store) = ((Return var), rest, store)

-------------------------------------------------------------------------------------------------

emptyList :: Eq t => [[t]] -> Bool
emptyList [] = False
emptyList (x:xs) | x == [] = True
                 | otherwise = emptyList xs

push :: String -> String -> Store -> Store
push intVar listVar store = (listVar,show newList):store
    where newList = let a = read (lookupVar intVar store) :: Int in
                    let b = read (lookupVar listVar store) :: [Int] in
                      a:b

varTaken :: String -> Store -> Bool
varTaken var [] = False
varTaken var ((n,x):xs) | n == var = True
                        | otherwise = varTaken var xs

pop :: (String,Expr) -> Store -> Store
pop (x, (CInt y)) store = store
pop (x, (Add a b)) store = store
pop (x, (Sub a b)) store = store
pop (x, (Mult a b)) store = store
pop (x, Pop y) ((a,b):xs) | a == y = (a, show $ tail $ (read b :: [Int])) : xs
                          | otherwise = (a,b) : pop (x, Pop y) xs


replace :: (String,String) -> Store -> Store
replace (x,v) ((a,b):xs) | x == a = ((a,v):xs)
                         | otherwise = (a,b) : replace (x,v) xs

evalExpr :: Store -> Expr -> Int
evalExpr store (CInt x) = x
evalExpr store (Var x) = read $ lookupVar x store
evalExpr store (Add var1 var2) = evalExpr store var1 + evalExpr store var2
evalExpr store (Sub var1 var2) = evalExpr store var1 - evalExpr store var2
evalExpr store (Mult var1 var2) = evalExpr store var1 * evalExpr store var2
evalExpr store (Pop list) = head $ read list'
    where list' = lookupVar list store

evalL :: Store -> Expr -> [Int]
evalL store (CList l) = read l
evalL store (Get i mVar) | length x > (evalExpr store i) = x !! (evalExpr store i)
                         | otherwise =  error "Index Greater Than List Length"
                           where x = read $ lookupVar mVar store

evalBool :: Store -> Expr -> Expr
evalBool store (CTrue) = CTrue
evalBool store (CFalse) = CFalse
evalBool store (Var x) | lookupVar x store == "CTrue" = evalBool store CTrue
                       | otherwise = evalBool store CFalse

evalBool store (Empty lVar) | lookupVar lVar store == "[]" = CTrue
                            | otherwise = CFalse


evalLoop :: State -> String
evalLoop state = lookupVar var store
   where ((Return var), rest, store) = eval state




