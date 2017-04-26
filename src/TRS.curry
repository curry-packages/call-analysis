--- Definition of term rewriting systems

module TRS where

import List
import Char
import Names

----------------------------------------------------------------------------
-- definition of basic terms
data Term = Var Int | Func FuncType String [Term]

data FuncType = Def | Cons

-- abbreviations
func = Func Def
cons = Func Cons

-- get all variables in a term:
varsOf :: Term -> [Int]
varsOf (Var v) = [v]
varsOf (Func _ f args)
  | f == "LAMBDA" = let (largs,lexp) = splitAt (length args - 1) args
                     in varsOf (head lexp) \\ concatMap varsOf largs
  | f == "LET" 
   = let (bindings,lexp) = splitAt (length args - 1) args
         bvars = concatMap (\ (Func _ _ [bvar,_]) -> varsOf bvar) bindings
      in varsOf (head lexp) \\ bvars
  | otherwise = foldr union [] (map varsOf args)

-- get all operation-rooted subterms of a term
-- (as a list of function name/argument pairs):
fSubterms :: Term -> [(String,[Term])]
fSubterms (Var _) = []
fSubterms (Func ft f args) = let argterms = concatMap fSubterms args in
  if ft==Def then (f,args) : argterms else argterms

-- get all operations occurring in term:
funcsInTerm :: Term -> [String]
funcsInTerm (Var _) = []
funcsInTerm (Func ft f args) = let argfuns = concatMap funcsInTerm args in
  if ft==Def then f : argfuns else argfuns

-- show a term (first argument: enclose in parentheses?)
showBasicTerm :: Bool -> Term -> String
showBasicTerm _ (Var i) = 'x' : show i
showBasicTerm _ (Func _ f []) = showOperator f
showBasicTerm withp (Func _ f [x]) =
  encloseInPar withp (showOperator f ++ ' ':showBasicTerm True x)
showBasicTerm withp (Func _ f args@(_:_:_))
  | f == "LET"    = encloseInPar withp ("let\n" ++ showLetExp args)
  | f == "LAMBDA" = encloseInPar withp (showLambdaExp args)
  | take 2 f == "(,"
   = '(' : concat (intersperse "," (map (showBasicTerm False) args)) ++ ")"
  | not (isAlpha (head f)) && length args == 2
   = encloseInPar withp
                  (showBasicTerm True (head args) ++ ' ':f ++
                     ' ':showBasicTerm True (args!!1))
  | otherwise
   = encloseInPar withp (showOperator f ++
                         concatMap (\a -> ' ':showBasicTerm True a) args)

showLetExp es = let (bindings,exp) = splitAt (length es - 1) es in
  concatMap showBinding bindings ++
  " in " ++ showBasicTerm False (head exp)
 where
  showBinding (Func _ _ [bvar,bexp]) =
    "  "++ showBasicTerm False bvar ++" = "++ showBasicTerm False bexp ++"\n"

showLambdaExp es = let (args,exp) = splitAt (length es - 1) es in
  "\\" ++ concatMap (\a -> showBasicTerm True a ++" ") args ++
  "-> " ++ showBasicTerm False (head exp)

showOperator f | isDigit (head f)  = f
               | head f == '\''    = f
               | f == "[]"         = f
               | isAlpha (head cf) = cf
               | otherwise         = '(':cf++")"
 where cf = genCorrectIdentifier f

encloseInPar withp s = if withp then '(' : s ++ ")" else s

----------------------------------------------------------------------------
-- definition of rules (term rewriting systems)

-- Rule for a function: (Rule f args rhs)
data Rule = Rule String [Term] Term

-- show a rule
showRule :: Rule -> String
showRule (Rule f args rhs) =
  showBasicTerm False (Func Def f args) ++ " = " ++ showBasicTerm False rhs ++
  if null extraVars then ""
  else " where " ++
       concat (intersperse ","
                  (map (\i -> showBasicTerm False (Var i)) extraVars)) ++
       " free"
 where
  extraVars = varsOf rhs \\ concatMap varsOf args

-- show a TRS
showTRS :: [Rule] -> String
showTRS = concat . intersperse "\n" . map showRule . filter isShowRule
 where
   isShowRule (Rule f _ _) = f `notElem` ["?"]

-- get all function/arity pairs defined by a list of rules:
allFunctions :: [Rule] -> [(String,Int)]
allFunctions rules = nub (map funOf rules)
 where funOf (Rule rf args _) = (rf,length args)

-- get all rules for a given function name:
funcRules :: String -> [Rule] -> [([Term],Term)]
funcRules _ [] = []
funcRules f (Rule rf args rhs : rules) =
    if f==rf then (args,rhs) : funcRules f rules
             else funcRules f rules

-- get the arity of a specific function from the rules:
arityOf :: String -> [Rule] -> Int
arityOf f [] = error $ "arity of function "++f++" not found"
arityOf f (Rule rf args _ : rules) =
  if f==rf then length args
           else arityOf f rules

-- does a rule contains a call to choice operator "?"?
containsChoice :: Rule -> Bool
containsChoice (Rule _ _ rhs) = hasChoice rhs
 where
  hasChoice (Var _) = False
  hasChoice (Func ft f args) = (ft==Def && f=="?") || any hasChoice args

-- rules defining the choice operator "?"
addChoiceRules :: [Rule] -> [Rule]
addChoiceRules rules =
  if ("?",2) `elem` allFunctions rules
  then rules
  else Rule "?" [Var 0, Var 1] (Var 0) :
       Rule "?" [Var 0, Var 1] (Var 1) : rules

-- does a rule contains a call to "apply"?
containsApply :: Rule -> Bool
containsApply (Rule _ _ rhs) = hasApply rhs
 where
  hasApply (Var _) = False
  hasApply (Func ft f args) = (ft==Def && f=="apply") || any hasApply args

-- add primitive apply rules to a TRS
addApplyRules :: [Rule] -> [Rule]
addApplyRules rules = rules ++ concatMap genAllApply funcArities
 where
  funcArities = nub (map (\ (Rule f args _) -> (f,length args)) rules)

  genAllApply (f,n) = map genApplyRule [1..n]
   where
    genApplyRule i =
     let pargs = map Var [1..(i-1)]
      in Rule "apply" [Func Cons f pargs, Var i]
              (Func (if i==n then Def else Cons) f (pargs++[Var i]))

----------------------------------------------------------------------------
