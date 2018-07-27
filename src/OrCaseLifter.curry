------------------------------------------------------------------------------
--- Transform FlatCurry function definitions into a form where all the
--- right-hand sides contain Case and Or only at the top-level and all
--- case expressions have a variable argument
--- (i.e., all rules are overlapping inductively sequential).
--- Furthermore, non-recursive Let bindings are replaced
--- by auxiliary functions.
---
--- @author Michael Hanus
--- @version May 2017
------------------------------------------------------------------------------


module OrCaseLifter(liftNestedOrCase) where

import FlatCurry.Types as FC
import Data.List


--- Lift nested or/case expressions by replacing them by auxiliary functions.
--- The names of the new auxiliary functions are prefixed by a string
--- and a number (starting with 0).
--- @param prefix - the prefix for the new auxiliary functions (e.g.,
---                 ("module","ORCASE_")
--- @param funs - the list of functions to be lifted
--- @return the list of lifted functions and auxiliary functions

liftNestedOrCase :: QName -> [FuncDecl] -> [FuncDecl]
liftNestedOrCase prefix funcs = fst (liftNestedOrCaseI prefix 0 funcs)

liftNestedOrCaseI _ idx [] = ([],idx)
liftNestedOrCaseI prefix idx (Func f@(fm,fn) n v t (Rule lvars rhs) : fs) =
  let fprefix = (fm,fn++"#")
      (newrhs,newffuns,newidx) = liftNestedOrCaseExp fprefix Top idx rhs
      (newfsfuns,newidx1) = liftNestedOrCaseI prefix newidx (fs++newffuns)
   in (Func f n v t (Rule lvars newrhs):newfsfuns, newidx1)
liftNestedOrCaseI prefix idx (Func f n v t (External fe) : fs) =
  let (newfsfuns,newidx) = liftNestedOrCaseI prefix idx fs
   in (Func f n v t (External fe):newfsfuns,newidx)

data ExpMode = Top | InOr | InCase | Inside
 deriving Eq

liftNestedOrCaseExp :: QName -> ExpMode -> Int -> Expr -> (Expr,[FuncDecl],Int)
liftNestedOrCaseExp _ _ idx (Var v) = (Var v,[],idx)
liftNestedOrCaseExp _ _ idx (Lit l) = (Lit l,[],idx)
liftNestedOrCaseExp prefix _ idx (Comb ct cf exps) =
       let (newexps,newfuns,newidx) = liftNestedOrCaseExps prefix Inside idx exps
        in (Comb ct cf newexps,newfuns,newidx)
liftNestedOrCaseExp prefix mode idx (FC.Or e1 e2) =
  if mode==Top || mode==InOr
  then let (newe1,newfuns1,idx1) = liftNestedOrCaseExp prefix InOr idx e1
           (newe2,newfuns2,idx2) = liftNestedOrCaseExp prefix InOr idx1 e2
        in (FC.Or newe1 newe2,newfuns1++newfuns2,idx2)
  else let fvars = nub (freeVarsInExp e1 ++ freeVarsInExp e2)
           auxfname = (fst prefix, snd prefix ++"OR"++show idx)
        in (Comb FuncCall auxfname (map Var fvars),
            [Func auxfname (length fvars) Private
                  (genPolyType (length fvars)) -- TODO: insert real type
                  (Rule fvars (FC.Or e1 e2))],
            idx+1)
liftNestedOrCaseExp prefix mode idx (Case ct e brs) =
  if (mode==Top || mode==InCase) && isVar e
  then let (newe,newfuns1,idx1) = liftNestedOrCaseExp prefix Inside idx e
           (newbrs,newfuns2,idx2) = liftNestedOrCaseBranches prefix idx1 brs
        in (Case ct newe newbrs,newfuns1++newfuns2,idx2)
  else let fvars = nub (concatMap freeVarsInBranch brs)
           auxfname = (fst prefix, snd prefix ++"CASE"++show idx)
        in (Comb FuncCall auxfname (map Var fvars ++ [e]),
            [Func auxfname (length fvars + 1) Private
                  (genPolyType (length fvars + 1)) -- TODO: insert real type
                  (Rule (fvars++[999]) (Case ct (Var 999) brs))],--TODO:freshv.
            idx+1)
liftNestedOrCaseExp prefix _ idx (Free vs exp) =
       let (newexp,newfuns,newidx) = liftNestedOrCaseExp prefix Inside idx exp
        in (Free vs newexp,newfuns,newidx)
liftNestedOrCaseExp prefix _ idx (Let bs exp) =
  let (newbs,newfuns1,idx1) = liftNestedOrCaseBindings prefix idx bs
      (newexp,newfuns2,newidx) = liftNestedOrCaseExp prefix Inside idx1 exp
   in eliminateLetExp prefix (newfuns1++newfuns2) newidx newbs newexp

-- try to eliminate a non-recursive Let expression by introducing
-- auxiliary functions:
eliminateLetExp prefix newfuns idx bs exp =
  if recursiveBindings bs
  then let (nonrecbs,letrecexp) = transformLet2NestedLet bs exp in
       if null nonrecbs
       then (letrecexp, newfuns, idx) -- no Let elimination possible
       else let (newexp,newfuns1,newidx) =
                         liftNestedOrCaseExp prefix Inside idx letrecexp
             in eliminateLetExp prefix (newfuns++newfuns1) newidx nonrecbs newexp
  else let fvars = filter (`notElem` map fst bs) (nub (freeVarsInExp exp))
           auxfname = (fst prefix,
                       snd prefix ++"LET"++show (length fvars)++"_"++show idx)
       in
       (Comb FuncCall auxfname (map Var fvars ++ map snd bs),
       [Func auxfname (length fvars + length bs) Private
             (genPolyType (length fvars + length bs)) -- TODO: insert real type
             (Rule (fvars++map fst bs) exp)] ++ newfuns,
       idx+1)

-- try to replace a potentially recursive Let expression by nested
-- non-recursive Let expressions:
transformLet2NestedLet bs exp =
  let (nonrecbs,recbs) = splitBindings (map fst bs) bs
   in if null recbs then (nonrecbs,exp)
                    else (nonrecbs,Let recbs exp)
 where
  splitBindings _ [] = ([],[])
  splitBindings boundvars (binding:bindings) =
   let (nbs,rbs) = splitBindings boundvars bindings
    in if any (`elem` boundvars) (freeVarsInExp (snd binding))
       then (nbs,binding:rbs)
       else (binding:nbs,rbs)

-- Is a set of bindings recursive?
recursiveBindings :: [(Int,Expr)] -> Bool
recursiveBindings bs =
  any (`elem` (map fst bs)) ((concatMap freeVarsInExp (map snd bs)))

liftNestedOrCaseBindings _ idx [] = ([],[],idx)
liftNestedOrCaseBindings prefix idx ((v,e):bs) =
  let (newe,newefun,idx1) = liftNestedOrCaseExp prefix Inside idx e
      (newes,newesfun,idx2) = liftNestedOrCaseBindings prefix idx1 bs
   in ((v,newe):newes,newefun++newesfun,idx2)

liftNestedOrCaseExps _ _ idx [] = ([],[],idx)
liftNestedOrCaseExps prefix mode idx (e:es) =
  let (newe,newefun,idx1) = liftNestedOrCaseExp prefix mode idx e
      (newes,newesfun,idx2) = liftNestedOrCaseExps prefix mode idx1 es
   in (newe:newes,newefun++newesfun,idx2)

liftNestedOrCaseBranches _ idx [] = ([],[],idx)
liftNestedOrCaseBranches prefix idx (Branch p e:bs) =
  -- since case with literal patterns are translated into if-then-else,
  -- we also have to lift cases inside such literal pattern branches:
  let (newe,newefun,idx1) = liftNestedOrCaseExp prefix
                                (if isLiteralPattern p then Inside else InCase) idx e
      (newes,newesfun,idx2) = liftNestedOrCaseBranches prefix idx1 bs
   in (Branch p newe:newes,newefun++newesfun,idx2)

-- is this a case branch on literals?
isLiteralPattern (Pattern _ _) = False
isLiteralPattern (LPattern _)  = True

-- get all unbound variables of an expression:
freeVarsInExp :: Expr -> [Int]
freeVarsInExp (Var v) = [v]
freeVarsInExp (Lit _) = []
freeVarsInExp (Comb _ _ exps) = concatMap freeVarsInExp exps
freeVarsInExp (FC.Or e1 e2) = freeVarsInExp e1 ++ freeVarsInExp e2
freeVarsInExp (Case _ e bs) = freeVarsInExp e ++ concatMap freeVarsInBranch bs
freeVarsInExp (Let bs e) =
  filter (`notElem` (map fst bs)) (concatMap freeVarsInExp (e : map snd bs))
freeVarsInExp (Free vs exp) = filter (`notElem` vs) (freeVarsInExp exp)

freeVarsInBranch (Branch (Pattern _ vs) e) =
                                        filter (`notElem` vs) (freeVarsInExp e)
freeVarsInBranch (Branch (LPattern _) e) = freeVarsInExp e

-- is an expression a variable?
isVar :: Expr -> Bool
isVar e = case e of
            Var _ -> True
            _     -> False

-- generate a most general polymorphic type of the form "a1->a2->...->an->a0"
-- where all ai a pairwise different type variables:
genPolyType n =
  if n==0 then TVar 0
          else FuncType (TVar (n+1)) (genPolyType (n-1))

-- end of OrCaseLifter
