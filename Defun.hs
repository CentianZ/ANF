module Defun where

type Name = String

data Op = Plus | Minus | Times | Divide | Eq | Lt | Gt
    deriving (Show)  -- 添加 Show 实例派生

data Expr
     = Var Name
     | Val Int
     | Let Name Expr Expr
     | Call Expr [Expr]
     | Con Name [Expr]
     | If Expr Expr Expr
     | BinOp Op Expr Expr
     | Case Expr [CaseAlt]
     | Apply Lexpr [Expr]

data CaseAlt = IfCon Name [Name] Expr

data Function = MkFun Name -- function name
                      [Name] -- argument names
                      Expr -- function body

data Program = MkProg [Function] -- all the function definitions
                      Expr -- expression to evaluate
data Lexpr = Local Name 
-- Function definitions, written as syntax trees
-- Function representation

data Funs   
  = FN_f          -- Represents the function `f`
  deriving (Show)


globalNames :: [Name]
globalNames = ["double", "factorial", "fst", "snd", "sum", "testlist", "map"]

isGlobal :: Name -> Bool
isGlobal name = name `elem` globalNames

defunLocal :: Function -> Function
defunLocal (MkFun name args body) = MkFun name args (transform body)
  where
    transform :: Expr -> Expr
    transform (Call (Var f) [arg]) 
      | isGlobal f = Call (Var f) [transform arg]        -- If global, leave unchanged
      | otherwise  = Call (Var "apply") [Var f, arg] -- If local, use `apply`
    transform (Case expr alts) =
      Case (transform expr) (map transformAlt alts)     -- Process case alternatives
    transform (Con name args) =
      Con name (map transform args)                     -- Transform constructor arguments
    transform (BinOp op left right) =
      BinOp op (transform left) (transform right)       -- Transform binary operations
    transform other = other                             -- Leave other expressions unchanged

    transformAlt :: CaseAlt -> CaseAlt
    transformAlt (IfCon name vars expr) = IfCon name vars (transform expr)

allDefsDefunLocal :: [Function]
allDefsDefunLocal = map defunLocal allDefs

-- APPLY function for interpreting `Funs`
apply :: Funs -> Expr -> Expr
apply FN_f arg = Call (Var "f") [arg]  -- Call the actual function `f`

{-
double(val) = val * 2
-}
double_def
    = MkFun "double" ["val"]
        (BinOp Times (Var "val") (Val 2))

{-
factorial(x) = if x == 0
                  then 1
                  else x * factorial(x-1)
-}
factorial_def
    = MkFun "factorial" ["x"]
        (If (BinOp Eq (Var "x") (Val 0))
            (Val 1)
            (BinOp Times
                   (Var "x")
                   (Call (Var "factorial") [BinOp Minus (Var "x") (Val 1)])))

{-
fst(p) = case p of
              MkPair(x,y) -> x
-}
fst_def
    = MkFun "fst" ["p"]
        (Case (Var "p")
             [IfCon "MkPair" ["x", "y"] (Var "x")])

{-
snd(p) = case p of
              MkPair(x,y) -> y
-}
snd_def
    = MkFun "snd" ["p"]
        (Case (Var "p")
             [IfCon "MkPair" ["x", "y"] (Var "y")])

{-
sum(xs) = case xs of
               Nil -> 0
               Cons(y, ys) -> y + sum(ys)
-}
sum_def :: Function
sum_def
    = MkFun "sum" ["xs"]
        (Case (Var "xs")
              [IfCon "Nil" [] (Val 0),
               IfCon "Cons" ["y", "ys"]
                     (BinOp Plus (Var "y") (Call (Var "sum") [Var "ys"]))])

{-
testlist() = Cons 1 (Cons 2 Nil)
-}
testlist_def
    = MkFun "testlist" []
        (Con "Cons" [Val 1, Con "Cons" [Val 2, Con "Nil" []]])

{-
map(f, xs) = case xs of
                  Nil -> Nil
                  Cons(y,ys) -> Cons(f(y), map(f,ys))
-}
map_def
    = MkFun "map" ["f", "xs"]
        (Case (Var "xs")
              [IfCon "Nil" [] (Con "Nil" []),
               IfCon "Cons" ["y", "ys"]
                     (Con "Cons" [Call (Var "f") [Var "y"],
                                  Call (Var "map") [Var "f", Var "ys"]])])

allDefs = [double_def, factorial_def, fst_def, snd_def, sum_def,
           testlist_def, map_def]


-- Should evaluate to 6
testProg1 = MkProg allDefs (Call (Var "double") [Val 3])

-- Should evaluate to 120
testProg2 = MkProg allDefs (Call (Var "factorial") [Val 5])

-- Should evaluate to 1
testProg3 = MkProg allDefs (Call (Var "fst") [Con "MkPair" [Val 1, Val 2]])

-- Should evaluate to 3
testProg4 = MkProg allDefs (Call (Var "sum") [Var "testlist"]) -- call
                      
-- Should evaluate to 6
testProg5 = MkProg allDefs
              (Call (Var "sum")
                [Call (Var "map") [Var "double", Var "testlist"]])

-- Should evaluate to 2
testProg6 = MkProg allDefs (Call (Var "snd") [Con "MkPair" [Val 1, Val 2]])

-- Should evaluate to 3
testProg7 = MkProg allDefs (Call (Var "sum") [Call (Var "testlist")[]]) -- call

program_defunLocal = MkProg allDefsDefunLocal (Call (Var "map") [Var "double", Var "testlist"])