-- Reading rules from an (Flat)Curry file:

module ReadFlatTRS(readRules,readRulesAndData,readFlatCurryRules) where

import qualified TRS
import FlatCurry.Types as FC
import FlatCurry.Files (readFlatCurry)
import OrCaseLifter

----------------------------------------------------------------------------
-- Reading rules from a (Flat)Curry file:
readRules :: String -> IO [TRS.Rule]
readRules prog = do
  putStrLn $ "Reading rules from Curry program " ++ prog ++ "..."
  flatprog <- readFlatCurry prog
  return (fst (curry2rules flatprog))

-- Reading rules and data declarations from a (Flat)Curry file:
readRulesAndData :: String -> IO ([TRS.Rule],[TypeDecl])
readRulesAndData prog = do
  putStrLn $ "Reading rules from Curry program " ++ prog ++ "..."
  flatprog <- readFlatCurry prog
  return (curry2rules flatprog)

-- Read FlatCurry program and return it together with the rules
-- transformed into a TRS:
readFlatCurryRules :: String -> IO (Prog,[TRS.Rule])
readFlatCurryRules prog = do
  putStrLn $ "Reading rules from Curry program " ++ prog ++ "..."
  flatprog <- readFlatCurry prog
  return (flatprog, fst $ curry2rules flatprog)

curry2rules (Prog modname _ tdecls fdecls _) =
  if any TRS.containsApply crules
  then (TRS.addApplyRules crules, tdecls)
  else (crules, tdecls)
 where
  rules  = concatMap fdecl2rules (liftNestedOrCase (modname,"ORCASE_") fdecls)
  crules = if any TRS.containsChoice rules then TRS.addChoiceRules rules
                                           else rules

-- translate function declaration into rules:
fdecl2rules (FC.Func (_,fname) arity _ _ (External _)) =
  [TRS.Rule fname (genArgs arity) (TRS.Func TRS.Def "EXTERNAL" [])]
 where
  genArgs n = map TRS.Var [1..n]

fdecl2rules (FC.Func fname _ _ _ (FC.Rule lhs rhs)) =
  map patternrule2rule patternrules
 where
  patternrules = rule2equations (Comb FuncCall fname (map Var lhs)) rhs

  patternrule2rule (l,r) =
    let (TRS.Func _ f args) = transExp l
     in TRS.Rule f args (transExp r)

  transExp (FC.Var i) = TRS.Var i
  transExp (Lit (Intc i))   = TRS.Func TRS.Cons (show i) [] 
  transExp (Lit (Floatc f)) = TRS.Func TRS.Cons (show f) []
  transExp (Lit (Charc c))  = TRS.Func TRS.Cons ['\'',c,'\''] []
  transExp (Comb ct (_,f) args) =
    TRS.Func (if ct==FuncCall then TRS.Def else TRS.Cons) f (map transExp args)
  transExp (Free _ exp) = transExp exp
  transExp (Let _ _)    = error "Let not yet supported"
  transExp (FC.Or _ _)  = error "Or not yet supported"
  transExp (Case _ _ _) = error "Case not yet supported"

----------------------------------------------------------------------------

-- transform a rule consisting of a left- and a right-hand side
-- (represented as expressions) into a set of pattern matching rules:
rule2equations :: Expr -> Expr -> [(Expr,Expr)]
rule2equations lhs (FC.Or e1 e2) =
   rule2equations lhs e1 ++ rule2equations lhs e2
rule2equations lhs (Case ctype e bs) =
   if isVarExpr e then let Var i = e  in  caseIntoLhs lhs i bs
                  else [(lhs,Case ctype e bs)]
rule2equations lhs (Var i) = [(lhs,Var i)]
rule2equations lhs (Lit l) = [(lhs,Lit l)]
rule2equations lhs (Comb ct name args) = [(lhs,Comb ct name args)]
rule2equations lhs (Free vs e) = [(lhs,Free vs e)]
rule2equations lhs (Let bs e) = [(lhs,Let bs e)]

caseIntoLhs _ _ [] = []
caseIntoLhs lhs vi (Branch (Pattern c vs) e : bs) =
  rule2equations (substitute [vi] [shallowPattern2Expr c vs] lhs) e
  ++ caseIntoLhs lhs vi bs
caseIntoLhs lhs vi (Branch (LPattern lit) e : bs) =
  rule2equations (substitute [vi] [Lit lit] lhs) e
  ++ caseIntoLhs lhs vi bs

shallowPattern2Expr name vars = Comb ConsCall name (map (\i->Var i) vars)


-- (substitute vars exps expr) = expr[vars/exps]
-- i.e., replace all occurrences of vars by corresponding exps in the
-- expression expr
substitute vars exps expr = substituteAll vars exps 0 expr

-- (substituteAll vars exps base expr):
-- substitute all occurrences of variables by corresonding expressions:
-- * substitute all occurrences of var_i by exp_i in expr
--   (if vars=[var_1,...,var_n] and exps=[exp_1,...,exp_n])
-- * substitute all other variables (Var j) by (Var (base+j))
--
-- here we assume that the new variables in guards and case patterns
-- do not occur in the list "vars" of replaced variables!

substituteAll :: [Int] -> [Expr] -> Int -> Expr -> Expr
substituteAll vars exps b (Var i) = replaceVar vars exps i
  where replaceVar [] [] var = Var (b+var)
        replaceVar (v:vs) (e:es) var = if v==var then e
                                                 else replaceVar vs es var
substituteAll _  _  _ (Lit l) = Lit l
substituteAll vs es b (Comb combtype c exps) =
                 Comb combtype c (map (substituteAll vs es b) exps)
substituteAll vs es b (Let bindings exp) =
                 Let (map (\(x,e)->(x+b,substituteAll vs es b e)) bindings)
                     (substituteAll vs es b exp)
substituteAll vs es b (Free vars e) =
                 Free (map (+b) vars) (substituteAll vs es b e)
substituteAll vs es b (FC.Or e1 e2) =
                 FC.Or (substituteAll vs es b e1) (substituteAll vs es b e2)
substituteAll vs es b (Case ctype e cases) =
   Case ctype (substituteAll vs es b e) (map (substituteAllCase vs es b) cases)

substituteAllCase vs es b (Branch (Pattern l pvs) e) =
                 Branch (Pattern l (map (+b) pvs)) (substituteAll vs es b e)
substituteAllCase vs es b (Branch (LPattern l) e) =
                 Branch (LPattern l) (substituteAll vs es b e)


-- Is the expression a guarded expressions?
isGuardedExpr :: Expr -> Bool
isGuardedExpr e = case e of
  Comb _ f _ -> f == ("Prelude","cond")
  _ -> False

-- Is the expression a variable?
isVarExpr :: Expr -> Bool
isVarExpr e = case e of
  Var _ -> True
  _ -> False


---------------------------------------------------------------------------
