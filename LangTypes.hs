module LangTypes where

import Grammar

type Store = [(String,String)] 

typeOf :: Store -> Expr -> LangType
typeOf store (CInt x) = TInt
typeOf store (Add x y) = TInt
typeOf store (Mult x y) = TInt
typeOf store (Pop x) = TInt
typeOf store (Sub x y) = TInt
typeOf store (Get x y) = TList
typeOf store (CList x) = TList
typeOf store CTrue = TBool
typeOf store CFalse = TBool
typeOf store GetLists = TMatrix
typeOf store (Var x) | s == "CTrue" = TBool
                     | s == "CFalse" = TBool
                     | length s > 1 && s !! 1 == '[' = TMatrix
                     | head s == '[' = TList
                     | otherwise = TInt
                         where s = (lookupVar x store)


lookupVar :: String -> Store -> String
lookupVar var [] = error "no bindings"
lookupVar var ((variable,x):xs) | var == variable = x
                                | otherwise = lookupVar var xs



