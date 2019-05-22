-- Haskell parser for FJ + Lambda
-- Author: Samuel da Silva Feitosa
-- Date: 03/2018
---------------------------------
module FJParser where
import Data.Map

-- FJ + Lambda syntactic constructors
-------------------------------------
data T = TClass Class
       | TInterface Interface
       deriving (Eq)

-- class C extends D implements I_ { T_ f_; K M_ }
data Class = Class String String [String] [(Type,String)] Constr [Method]
              deriving (Eq)

-- Interface I extends I_ { S_ default M_ }
data Interface = Interface String [String] [Sign] [Method]
               deriving (Eq)

-- C(T_ f_) { super(f_); this.f_.f_; }
data Constr = Constr String [(Type,String)] [String] [(String,String)]
            deriving (Eq)

-- T m(T_ x_)
data Sign = Sign Type String [(Type,String)]
          deriving (Eq)

-- S { return e; }
data Method = Method Sign Expr
            deriving (Eq)

data Expr = Var String                               -- Variable
          | FieldAccess Expr String                  -- Field Access
          | MethodInvk Expr String [Expr]            -- Method Invocation
          | CreateObject String [Expr]               -- Object Instantiation
          | Cast String Expr                         -- Cast
          | Closure [(Type,String)] Expr             -- Closure
          deriving (Eq)

-- FJ + Lambda nominal typing
-----------------------------
data Type = Type String
          deriving (Eq)

-- FJ + Lambda auxiliary definitions
------------------------------------
type Env = Map String Type
type CT = Map String T

-- FJ + Lambda typing errors
----------------------------
data TypeError = VariableNotFound String
               | FieldNotFound String
               | ClassNotFound String
               | MethodNotFound String String
               | ParamsTypeMismatch [(Expr,Type)]
               | WrongClosure String Expr
               | WrongCast String Expr
               | UnknownError Expr
               deriving (Show,Eq)


-------------------------------------------------------------------------------
----------------------------- FORMATTING FUNCTIONS ----------------------------
-------------------------------------------------------------------------------

-- Function: showAttrs
-- Objective: Format attributes as in a Java source-code.
-- Params: List of types and names.
-- Returns: A formatted string.
---------------------------------------------------------
showAttrs :: [(Type,String)] -> String
showAttrs [] = ""
showAttrs ((t,s):hs) = show t ++ " " ++ s ++ ";\n  " ++ showAttrs hs

-- Function: showFormalParams
-- Objective: Format formal parameters as a Java method.
-- Params: List of types and names.
-- Returns: A formatted string.
--------------------------------------------------------
showFormalParams :: [(Type,String)] -> String
showFormalParams [] = ""
showFormalParams [(t,s)] = show t ++ " " ++ s
showFormalParams ((t,s):hs) = show t ++ " " ++ s ++ ", " ++ showFormalParams hs

-- Function: showAttrAssign
-- Objective: Format the constructor assignments.
-- Params: A list containing the member names.
-- Returns: A formatted string.
-------------------------------------------------
showAttrAssign :: [(String,String)] -> String
showAttrAssign [] = ""
showAttrAssign [(f,s)] = "    this." ++ f ++ " = " ++ s ++ ";"
showAttrAssign ((f,s):hs) = "    this." ++ f ++ " = " ++ s ++ ";\n" ++ 
                              showAttrAssign hs

-- Function: showParams
-- Objective: Format the actual parameters as expected by a method invocation.
-- Params: A list containing the variable names.
-- Returns: A formatted string.
-------------------------------------------------------------------------------
showParams :: [String] -> String
showParams [] = ""
showParams [s] = s
showParams (h:hs) = h ++ "," ++ showParams hs

-- Function: showInterfaces
-- Objective:
-- Params:
-- Returns:
--------------------------------------------------------------------------------
showInterfaces :: [String] -> String -> String
showInterfaces [] _ = ""
showInterfaces l kw = kw ++ " " ++ showParams l

-- Function: showSigns
-- Objective: Format a signature list.
-- Params: List of signatures.
-- Returns: A formatted string.
--------------------------------------
showSigns :: [Sign] -> String
showSigns [] = ""
showSigns [s] = "  " ++ show s ++ ";\n"
showSigns (h:hs) = "  " ++ show h ++ ";\n" ++ showSigns hs

-- Function: showMethods
-- Objective: Format a method list.
-- Params: List of methods.
-- Returns: A formatted string.
-----------------------------------
showMethods :: [Method] -> String -> String
showMethods [] _ = ""
showMethods [m] kw = "  " ++ kw ++ show m
showMethods (h:hs) kw = "  " ++ kw ++ show h ++ "\n" ++ showMethods hs kw

-- Function: showExprs
-- Objective: Format a list of expressions.
-- Params: List of expressions.
-- Returns: A formatted string.
-------------------------------------------
showExprs :: [Expr] -> String
showExprs [] = ""
showExprs [e] = show e
showExprs (h:hs) = show h ++ "," ++ showExprs hs

-- Function: formatCT
-- Objective: Format a class table as a sequence of classes.
-- Params: A list of classes.
-- Returns: A string containing a formatted list of classes.
------------------------------------------------------------
formatCT :: [T] -> String
formatCT [] = "\n"
formatCT (h:hs) = (show h) ++ "\n" ++ formatCT hs

-- Function: formatExpr
-- Objective: Generate the source-code of the main class.
-- Params: An expression.
-- Returns: A string containing the source-code of the main class.
------------------------------------------------------------------
formatExpr :: Expr -> String
formatExpr e = "class FJMain {\n" ++ 
               "  public static void main(String args[]) {\n" ++
               "    System.out.println((" ++ show e ++ ").toString());\n" ++
               "  }\n" ++
               "}\n"
               
-- Function: formatJavaProgram
-- Objective: Generate a string containing a complete Java program.
-- Params: Class table, Expression.
-- Returns: A string containing a compilable Java program.
-------------------------------------------------------------------
formatJavaProgram :: CT -> Expr -> String
formatJavaProgram ct e = formatCT (elems ct) ++ "\n" ++ formatExpr e

-------------------------------------------------------------------------------
----------------------------- FORMATTING INSTANCES ----------------------------
-------------------------------------------------------------------------------

-- Formatting types
-------------------
instance Show Type where
  show (Type t) = t

instance Show T where
  show (TClass c) = show c
  show (TInterface i) = show i

-- Formatting classes
---------------------
instance Show Class where
  show (Class c b il at cn m) = "class " ++ c ++ " extends " ++ b ++ 
    " " ++ showInterfaces il "implements" ++ " {\n  " ++ 
    showAttrs at ++ show cn ++ "\n" ++ showMethods m "public " ++ "\n}"

-- Formatting interfaces
------------------------
instance Show Interface where
  show (Interface i il s m) = "interface " ++ i ++ " " ++ 
    showInterfaces il "extends" ++ " {\n" ++ showSigns s ++ 
    showMethods m "default " ++ "\n}"

-- Formatting constructors
--------------------------
instance Show Constr where
  show (Constr c fp at ths) = c ++ "(" ++ showFormalParams fp ++ 
    ") {\n    super(" ++ showParams at ++ ");\n" ++ 
    showAttrAssign ths ++ "\n  }"

-- Formatting signatures
------------------------
instance Show Sign where
  show (Sign r m fp) = show r ++ " " ++ m ++ "(" ++ showFormalParams fp ++ ")"

-- Formatting methods
---------------------
instance Show Method where
  show (Method s e) = show s ++ " {\n    return " ++ show e ++ ";\n  }"

-- Formatting expressions
-------------------------
instance Show Expr where
  show (Var v) = v
  show (FieldAccess e f) = show e ++ "." ++ f
  show (MethodInvk e m p) = show e ++ "." ++ m ++ "(" ++ showExprs p ++ ")"
  show (CreateObject c p) = "new " ++ c ++ "(" ++ showExprs p ++ ")"
  show (Cast c e) = "((" ++ c ++ ") " ++ show e ++ ")"
  show (Closure fp e) = "(" ++ showFormalParams fp ++ ") -> { return " ++ show e ++ "; }" 

