-----------------------------------------------------------------------------
-- A few base functions for analysing dependencies in FlatCurry programs:
--
-- Michael Hanus, June 2005
-----------------------------------------------------------------------------

module Dependency(analyseWithDependencies, indirectlyDependent,
                  funcsInExpr, callsDirectly, externalDependent,
                  dependencyGraphs, localDependencyGraphs) where

import FlatCurry.Types
import Data.List
import Data.Maybe (fromJust)
import qualified Data.Set.RBTree as RBT
import Sort       (leqString)

-- Generic global function analysis where the property of each function is a combination
-- of a property of the function and all its dependent functions.
-- 1. parameter: a function that associates a property to each function declaration
-- 2. parameter: an operation to combine the properties of function/dependent functions
analyseWithDependencies :: (FuncDecl->a) -> ([a]->a) -> [FuncDecl] -> [(QName,a)]
analyseWithDependencies funproperty combine funs = map anaFun alldeps
  where
    anaFun (name,depfuns) = (name, combine (map (lookupProp funprops) (name:depfuns)))

    funprops = map (\f->(funcName f, funproperty f)) funs

    alldeps = indirectlyDependent funs

    lookupProp :: [(QName,a)] -> QName -> a
    lookupProp fprops fun = fromJust (lookup fun fprops)

    funcName (Func fname _ _ _ _) = fname


-- external functions on which a function depends
externalDependent :: [FuncDecl] -> [(QName,[QName])]
externalDependent funcs =
  map (\ (f,fs)->(f,filter (`elem` externalFuncs) fs))
      (indirectlyDependent funcs)
 where
   externalFuncs = concatMap getExternal funcs

   getExternal (Func _ _ _ _ (Rule _ _)) = []
   getExternal (Func f _ _ _ (External _)) = [f]


-- Computes the list of indirect dependencies for all functions.
-- Argument: a list of function declarations
-- Result: a list of pairs of qualified functions names and the corresponding
--         called functions
indirectlyDependent :: [FuncDecl] -> [(QName,[QName])]
indirectlyDependent funs = map (\ (f,ds) -> (f,RBT.toList ds))
                               (depsClosure (map directlyDependent funs))

-- list of direct dependencies for a function
callsDirectly :: FuncDecl -> [QName]
callsDirectly fun = RBT.toList (snd (directlyDependent fun))

-- set of direct dependencies for a function
directlyDependent :: FuncDecl -> (QName,RBT.SetRBT QName)
directlyDependent (Func f _ _ _ (Rule _ e))   = (f,funcSetOfExpr e)
directlyDependent (Func f _ _ _ (External _)) = (f,emptySet)

-- compute the transitive closure of all dependencies based on a list of
-- direct dependencies:
depsClosure :: [(QName,RBT.SetRBT QName)] -> [(QName,RBT.SetRBT QName)]
depsClosure directdeps = map (\(f,ds)->(f,closure ds (RBT.toList ds)))
                             directdeps
 where
  closure olddeps [] = olddeps
  closure olddeps (f:fs) =
     let newdeps = filter (\e->not (RBT.member e olddeps))
                          (RBT.toList (maybe emptySet id (lookup f directdeps)))
      in closure (foldr RBT.insert olddeps newdeps) (newdeps++fs)

-- Computes the list of all direct dependencies for all functions.
-- This is useful to represent the dependency graph for each function.
-- Argument: a list of function declarations
-- Result: a list of pairs of qualified functions names and the corresponding list of
--         direct dependencies for all functions on which this functions depend
dependencyGraphs :: [FuncDecl] -> [(QName,[(QName,[QName])])]
dependencyGraphs funs =
  let directdeps = map directlyDependent funs
   in map (\(f,ds) -> (f,map (\g->(g,RBT.toList (fromJust (lookup g directdeps))))
                             (RBT.toList (RBT.insert f ds))))
          (depsClosure directdeps)

-- Computes for all functions the list of all direct local dependencies, i.e.,
-- dependencies occurring in the module where the function is defined.
-- Thus, dependencies outside the module are not represented.
-- This is useful to represent the local dependency graph for each function.
-- Argument: a list of function declarations
-- Result: a list of pairs of qualified functions names and the corresponding list of
--         direct local dependencies for all functions on which this functions depend
localDependencyGraphs :: [FuncDecl] -> [(QName,[(QName,[QName])])]
localDependencyGraphs funs =
  let directdeps = map directlyDependent funs
   in map (\(f,ds) -> (f,map (\g->(g,if fst f == fst g
                                     then RBT.toList (fromJust (lookup g directdeps))
                                     else []))
                             (RBT.toList (RBT.insert f ds))))
          (localDepsClosure directdeps)

-- compute the transitive closure of all local dependencies based on a list of
-- direct dependencies:
localDepsClosure :: [(QName,RBT.SetRBT QName)] -> [(QName,RBT.SetRBT QName)]
localDepsClosure directdeps =
  map (\(f,ds)->(f,closure (fst f) ds (RBT.toList ds))) directdeps
 where
  closure _ olddeps [] = olddeps
  closure mod olddeps (f:fs)
   | mod == fst f  -- f is local in this module: add dependencies
    = let newdeps = filter (\e->not (RBT.member e olddeps))
                           (RBT.toList (maybe emptySet id (lookup f directdeps)))
       in closure mod (foldr RBT.insert olddeps newdeps) (newdeps++fs)
   | otherwise = closure mod olddeps fs

-- Gets a list of all functions (including partially applied functions)
-- called in an expression:
funcsInExpr :: Expr -> [QName]
funcsInExpr e = RBT.toList (funcSetOfExpr e)

-- Gets the set of all functions (including partially applied functions)
-- called in an expression:
funcSetOfExpr :: Expr -> RBT.SetRBT QName
funcSetOfExpr (Var _) = emptySet
funcSetOfExpr (Lit _) = emptySet
funcSetOfExpr (Comb ct f es) =
  if isConstructorComb ct then unionMap funcSetOfExpr es
                          else RBT.insert f (unionMap funcSetOfExpr es)
funcSetOfExpr (Free _ e) = funcSetOfExpr e
funcSetOfExpr (Let bs e) = RBT.union (unionMap (funcSetOfExpr . snd) bs) (funcSetOfExpr e)
funcSetOfExpr (Or e1 e2) = RBT.union (funcSetOfExpr e1) (funcSetOfExpr e2)
funcSetOfExpr (Case _ e bs) = RBT.union (funcSetOfExpr e) (unionMap funcSetOfBranch bs)
                     where funcSetOfBranch (Branch _ be) = funcSetOfExpr be

isConstructorComb ct = case ct of
  ConsCall       -> True
  ConsPartCall _ -> True
  _              -> False

unionMap f = foldr RBT.union RBT.empty . map f

emptySet = RBT.empty

leqQName (m1,n1) (m2,n2) = leqString (m1++('.':n1)) (m2++('.':n2))

-- end of Dependency
