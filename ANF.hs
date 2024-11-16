module ANF where

import Control.Monad.State
import Defun
import Data.List (intercalate)

-- ANF 数据类型定义
data ANFExpr
    = AVar Name
    | AVal Int
    | ALet Name ANFExpr ANFExpr
    | ACall Name [Name]
    | ACon Name [Name]
    | AIf Name ANFExpr ANFExpr
    | ABinOp Op Name Name
    | ACase Name [ACaseAlt]
    -- | Apply Lexpr [Name]
    deriving (Show)

data ACaseAlt = AIfCon Name [Name] ANFExpr
     deriving (Show)  -- 自动派生 Show 实例

data ANFFunction = MkAFun { fnName :: Name,
                            argNames :: [Name],
                            definition :: ANFExpr }
                            deriving (Show)  -- 自动派生 Show 实例

data ANFProgram = MkProg { defs :: [ANFFunction],
                           mainExpr :: ANFExpr }
                           deriving (Show)  -- 自动派生 Show 实例

progToANF :: Program -> ANFProgram
progToANF (Defun.MkProg funcs mainExpr) =
    ANF.MkProg (map funcToANF funcs) (exprToANF mainExpr)

funcToANF :: Function -> ANFFunction
funcToANF (MkFun name args body) =
    let anfBody = exprToANF body
    in MkAFun { fnName = name, argNames = args, definition = anfBody }

-- Top-level function that initiates ANF conversion with an initial counter of 0
exprToANF :: Expr -> ANFExpr
exprToANF expr = fst $ exprToANF' expr 0

-- Helper function that does the actual ANF conversion
-- Takes an expression and a counter, returns an ANF expression and an updated counter
exprToANF' :: Expr -> Int -> (ANFExpr, Int)
exprToANF' (Var name) counter = (AVar name, counter)
exprToANF' (Val value) counter = (AVal value, counter)
exprToANF' (BinOp op e1 e2) counter =
    let
        -- Convert the left-hand side expression to ANF
        (e1', counter1) = exprToANF' e1 counter
        -- Generate a fresh variable for the left-hand side
        name1 = "t" ++ show counter1
        counter2 = counter1 + 1

        -- Convert the right-hand side expression to ANF
        (e2', counter3) = exprToANF' e2 counter2
        -- Generate a fresh variable for the right-hand side
        name2 = "t" ++ show counter3
        counter4 = counter3 + 1

        -- Create the resulting ANF expression with let bindings
        anfExpr = ALet name1 e1' (ALet name2 e2' (ABinOp op name1 name2))
    in
        (anfExpr, counter4)
-- Handling If expression
exprToANF' (If condExpr thenExpr elseExpr) counter =
    let
        -- Convert the condition expression to ANF
        (condExpr', counter1) = exprToANF' condExpr counter
        -- Generate a fresh variable for the condition
        condVar = "t" ++ show counter1
        counter2 = counter1 + 1

        -- Convert the "then" branch to ANF
        (thenExpr', counter3) = exprToANF' thenExpr counter2

        -- Convert the "else" branch to ANF
        (elseExpr', counter4) = exprToANF' elseExpr counter3

        -- Create the resulting ANF expression with let bindings
        anfExpr = ALet condVar condExpr' (AIf condVar thenExpr' elseExpr')
    in
        (anfExpr, counter4)
exprToANF' (Case scrutinee alts) counter =
    let
        -- Convert the scrutinee (the variable being matched) to ANF
        (scrutinee', counter1) = exprToANF' scrutinee counter
        scrutineeVar = "t" ++ show counter1
        counter2 = counter1 + 1

        -- Convert the alternatives
        anfAlts = map (convertAlt counter2) alts
    in
        (ALet scrutineeVar scrutinee' (ACase scrutineeVar anfAlts), counter2 + length alts)
  where
    -- Convert a single alternative
    convertAlt :: Int -> CaseAlt -> ACaseAlt
    convertAlt c (IfCon conName varNames body) =
        let
            (body', _) = exprToANF' body (c + length varNames)
        in
            AIfCon conName varNames body'
-- Handling Con constructor expression from left to right
-- Handling Con constructor expression from left to right
exprToANF' (Con conName args) counter =
    let
        -- Convert arguments from left to right
        (anfExpr, newCounter, argNames) = convertArgsFromHead args counter

        -- Create the ACon with all bound argument names, keeping the original order
        finalExpr = ACon conName argNames
    in
        (anfExpr finalExpr, newCounter)
  where
    -- Helper function to convert arguments from head (left to right)
    convertArgsFromHead :: [Expr] -> Int -> (ANFExpr -> ANFExpr, Int, [Name])
    convertArgsFromHead [] c = (\e -> e, c, [])
    convertArgsFromHead (arg:rest) c =
        -- Process the current element, then recursively the rest
        let 
            -- Convert the current argument to ANF
            (argExpr, argCounter) = exprToANF' arg c
            argName = "t" ++ show argCounter
            newCounter = argCounter + 1

            -- Convert the rest of the arguments
            (restBinding, finalCounter, restNames) = convertArgsFromHead rest newCounter
        in
            (\e -> ALet argName argExpr (restBinding e), finalCounter, argName : restNames)  -- Add the current argument to the head of the list


-- Handling Call expressions

exprToANF' (Call (Var f) args) counter =
    let
        -- Convert each argument to ANF
        (anfArgs, finalCounter, argNames) = convertArgs args counter

        -- Create the ANF representation for the function call without extra binding for the function name
        anfExpr = createLetBindings anfArgs (ACall f argNames)
    in
        (anfExpr, finalCounter)

-- Handling Call expressions when the function is not a simple variable
exprToANF' (Call funExpr args) counter =
    let
        -- Convert the function itself to ANF and bind it to a new variable
        (anfFun, counter1) = exprToANF' funExpr counter
        funName = "t" ++ show counter1
        counter2 = counter1 + 1

        -- Convert each argument to ANF
        (anfArgs, finalCounter, argNames) = convertArgs args counter2

        -- Create the ANF representation for the function call with let bindings
        anfExpr = ALet funName anfFun (createLetBindings anfArgs (ACall funName argNames))
    in
        (anfExpr, finalCounter)

-- Helper function to convert arguments to ANF
convertArgs :: [Expr] -> Int -> ([(ANFExpr, Name)], Int, [Name])
convertArgs [] c = ([], c, [])
convertArgs (arg:rest) c =
    let
        -- Convert the current argument to ANF
        (argExpr, newCounter) = exprToANF' arg c
        argName = "t" ++ show newCounter
        nextCounter = newCounter + 1

        -- Convert the rest of the arguments
        (restExprs, finalCounter, names) = convertArgs rest nextCounter
    in
        ((argExpr, argName) : restExprs, finalCounter, argName : names)

-- Helper function to create let bindings from ANF arguments
createLetBindings :: [(ANFExpr, Name)] -> ANFExpr -> ANFExpr
createLetBindings [] finalExpr = finalExpr
createLetBindings ((argExpr, name):rest) finalExpr =
    ALet name argExpr (createLetBindings rest finalExpr)


anfToJava :: ANFProgram -> String
anfToJava (ANF.MkProg defs mainExpr) =
    let
        -- Generate Java code for each function in ANFProgram
        functionStrings = map generateJavaFunction defs
        -- Generate Java code for the main method to evaluate the main expression
        mainString = generateMainMethod mainExpr
    in
        unlines $
            [ "import java.util.*;"
            , "public class GeneratedProgram {"
            , ""
            , "    static class Constructor {"
            , "        String name;"
            , "        List<Object> args;"
            , ""
            , "        Constructor(String name, List<Object> args) {"
            , "            this.name = name;"
            , "            this.args = args;"
            , "        }"
            , ""
            , "        String getTag() { return name; }"
            , "        Object getArg(int index) { return args.get(index); }"
            , "    }"
            , ""
            ] ++ functionStrings ++ [mainString, "}"]

-- Helper to generate Java code for each ANF function
generateJavaFunction :: ANFFunction -> String
generateJavaFunction (MkAFun { fnName = name, argNames = args, definition = body }) =
    let
        -- Rename the function with prefix "my_"
        renamedName = "my_" ++ name

        -- Generate arguments and method header
        argsString = unwords $ map (\arg -> "Object " ++ arg) args
        header = "    public static Object " ++ renamedName ++ "(" ++ argsString ++ ") {"

        -- Declare res variable as Object type and initialize it to null
        resDeclaration = "        Object res = null;"

        -- Generate body with intermediate variables and return statement
        bodyString = generateJavaExpression body "res"

        -- Return the complete function definition
    in
        unlines [header, resDeclaration, bodyString, "        return res;", "    }"]

-- Helper to generate Java code for an ANF expression
generateJavaExpression :: ANFExpr -> String -> String
generateJavaExpression (AVar name) resultVar = "        " ++ resultVar ++ " = " ++ name ++ ";"
generateJavaExpression (AVal value) resultVar = "        " ++ resultVar ++ " = Integer.valueOf(" ++ show value ++ ");"
generateJavaExpression (ALet name expr rest) resultVar =
    let
        -- Declare intermediate variables as Object type and initialize to null
        varDeclaration = "        Object " ++ name ++ " = null;"
        exprString = generateJavaExpression expr name
        restString = generateJavaExpression rest resultVar
    in
        unlines [varDeclaration, exprString, restString]
generateJavaExpression (ACall fnName args) resultVar =
    let
        -- Rename function with prefix "my_"
        renamedFnName = "my_" ++ fnName
        -- Combine arguments with comma separation
        argsString = intercalate ", " args
    in
        "        " ++ resultVar ++ " = " ++ renamedFnName ++ "(" ++ argsString ++ ");"
generateJavaExpression (AIf condExpr thenExpr elseExpr) resultVar =
    let
        -- Convert condition to boolean explicitly
        condString = "        if ((boolean) " ++ condExpr ++ ") {"
        thenString = generateJavaExpression thenExpr resultVar
        elseString = "} else {"
        elseBody = generateJavaExpression elseExpr resultVar
    in
        unlines [condString, thenString, elseString, elseBody, "        }"]
generateJavaExpression (ABinOp op left right) resultVar =
    let
        operationString = "        " ++ resultVar ++ " = (Integer) " ++ left ++ " " ++ showBinOp op ++ " (Integer) " ++ right ++ ";"
    in
        operationString
generateJavaExpression (ACon name args) resultVar =
    let
        argsString = "Arrays.asList(" ++ (intercalate ", " args) ++ ")"
    in
        "        " ++ resultVar ++ " = new Constructor(\"" ++ name ++ "\", " ++ argsString ++ ");"
generateJavaExpression (ACase scrutinee alts) resultVar =
    let
        scrutVar = "scrutVar"
        varDeclaration = "        Object " ++ scrutVar ++ " = null;"
        scrutineeString = generateJavaExpression (AVar scrutinee) scrutVar
        switchString = "        switch(((Constructor) " ++ scrutVar ++ ").getTag()) {"
        altsStrings = concatMap (generateCaseAlt resultVar scrutVar) alts
    in
        unlines [varDeclaration, scrutineeString, switchString, altsStrings, "        }"]


-- Helper to generate Java code for case alternatives
generateCaseAlt :: String -> String -> ACaseAlt -> String
generateCaseAlt resultVar scrutVar (AIfCon conName argNames body) =
    let
        caseHeader = "            case \"" ++ conName ++ "\":"
        argAssignments = unlines $ zipWith (\arg idx ->
            "                Object " ++ arg ++ " = ((Constructor) " ++ scrutVar ++ ").getArg(" ++ show idx ++ ");"
            ) argNames [0..]
        bodyString = generateJavaExpression body resultVar
    in
        unlines [caseHeader, argAssignments, bodyString, "                break;"]


-- Helper to generate the main method
generateMainMethod :: ANFExpr -> String
generateMainMethod mainExpr =
    let
        mainBodyString = generateJavaExpression mainExpr "result"
    in
        unlines
            [ "    public static void main(String[] args) {"
            , "        Object result;"
            , mainBodyString
            , "        System.out.println(result);"
            , "    }"
            ]

-- Helper to generate binary operator symbols
showBinOp :: Op -> String
showBinOp Plus = "+"
showBinOp Minus = "-"
showBinOp Times = "*"
showBinOp Divide = "/"
showBinOp Eq = "=="
showBinOp Lt = "<"
showBinOp Gt = ">"


toJava :: Program -> String
toJava p = anfToJava (progToANF p)

-- All being well, this will print out a java program which, when compiled,
-- will print the result of evaluating the expression in testProg1
main :: IO ()
main = putStrLn (toJava testProg1)