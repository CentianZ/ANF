-- TestANF.hs
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module TestANF where

import ANF
import Defun

 -- Test input
double_def = MkFun "double" ["val"] (BinOp Times (Var "val") (Val 2))

-- Convert to ANF
double_anf = funcToANF Defun.double_def

factorial_anf = funcToANF Defun.factorial_def

fst_and = funcToANF Defun.fst_def

snd_and = funcToANF Defun.snd_def

sum_and = funcToANF Defun.sum_def

testlist_and = funcToANF Defun.testlist_def
 
testProg = Defun.MkProg [Defun.double_def, Defun.fst_def, Defun.snd_def] -- 所有函数定义
                       (Call (Var "fst") [Con "MkPair" [Val 1, Val 2]])  -- 主表达式调用

-- Print the result
main :: IO ()
main = print double_anf
test = progToANF testProg
