----------------------------------------------------------------------------
--- Fixpoint analyzer for call patterns
---
--- @author Michael Hanus
--- @version April 2025
----------------------------------------------------------------------------

module Analysis where

import Data.List ( intercalate, nub, replace, sort, sortBy )
import Numeric(readNat)
--import Sort(sortBy,leqString,leqList)
import System.Environment ( getArgs )
import System.IO ( hFlush, stdout )

import System.Directory ( createDirectory, doesDirectoryExist )
import System.FilePath  ( dropExtension, splitFileName, takeDirectory )
--import IO
import FlatCurry.ShowIntMod

import Debug.Profile
import qualified FlatCurry.Types as FC
import qualified Data.Map as Map ( Map, empty, lookup, toList, insert )

import LetDropping
import NondetAnalysis
import ReadFlatTRS
import TRS

----------------------------------------------------------------------------
-- Directories and files to store analysis results
analysisDir :: String
analysisDir = ".curry/analysis"

-- Create analysis directory for a given base file name, if necessary.
createAnalysisDir :: String -> IO ()
createAnalysisDir base = do
  let anadir = takeDirectory base ++ "/" ++ analysisDir
  exanadir <- doesDirectoryExist anadir
  if exanadir then return ()
              else createDirectory anadir

-- Nondet info file for a given module
ndInfoFileName :: String -> String
ndInfoFileName file =
  let (dir,base) = splitFileName file
      basemod = dropExtension base
   in dir ++ "/" ++ analysisDir ++ "/" ++ basemod ++ ".ndinfo"

-- strictness info file for a given module
strictInfoFileName :: String -> String
strictInfoFileName file =
  let (dir,base) = splitFileName file
      basemod = dropExtension base
   in dir ++ "/" ++ analysisDir ++ "/" ++ basemod ++ ".strictinfo"

----------------------------------------------------------------------------
-- Structure to represent abstract domains.
-- An abstract domain has the structure
--   (ADom abottom avar acons matchterms domleq ordleq showaterm)
-- and contains the following components:
-- abottom   : abstract bottom element
-- avar      : maps a free variable (number) into an abstract term
-- acons     : maps a constructor and abstract terms into an abstract term
-- matchterms: abstract matching of terms
-- domleq    : information ordering on abstract terms
-- showaterm : show an abstract term
-- applyprim : possible application of predefined operations
--             (Nothing: not a predefined operation, Just abstract_result)

data ADom a = ADom a
                   (Int -> a)
                   (String -> [a] -> a)
                   ([Term] -> [a] -> Maybe (Sub a))
                   (a -> a -> Bool)
                   (a -> String)
                   (String -> [a] -> Maybe a)

-- Returns bottom element of abstract domain
adomBottom :: ADom a -> a
adomBottom (ADom abottom _ _ _ _ _ _) = abottom

-- Returns "show" operation of abstract domain
adomShow :: ADom a -> (a -> String)
adomShow (ADom _ _ _ _ _ ashow _) = ashow

-- map a function applied to constructor terms
-- into an abstract abstract call w.r.t. a given domain
abstractCall :: ADom a -> (String,[Term]) -> (String,[a])
abstractCall (ADom _ avar acons _ _ _ _) (f,cargs) = (f, map cons2aterm cargs)
 where
  -- map a constructor term into abstract term
  cons2aterm (Var v) = avar v
  cons2aterm (Func Cons c args) = acons c (map cons2aterm args)
  cons2aterm (Func Def _ _) = error "cons2aterm: Func Def occurred"

----------------------------------------------------------------------------
-- Semantic equation (part of an interpretation): f args = result
data SemEq aterm = Eq String [aterm] aterm
 deriving (Eq, Ord)

-- (Abstract) Interpretation: list of semantic equations
type SemInt aterm = [SemEq aterm]

-- Substitutions: first simple implementation as string/value pairs
type Sub a = [(Int,a)]

---------------------------------------------------------------------------
-- Concrete domain: constructor terms

-- Constructor terms with bottom elements:
data CTerm = CBot | CVar Int | CCons String [CTerm]
 deriving (Eq, Ord)

-- pairwise matching of a list of patterns against a list of terms
matchCTerms :: [Term] -> [CTerm] -> Maybe (Sub CTerm)
matchCTerms []     []     = Just []
matchCTerms []     (_:_)  = Nothing
matchCTerms (_:_)  []     = Nothing
matchCTerms (x:xs) (y:ys) = combineSubst (match x y) (matchCTerms xs ys)
 where
  combineSubst Nothing   _         = Nothing
  combineSubst (Just _ ) Nothing   = Nothing
  combineSubst (Just s1) (Just s2) = Just (s1++s2)

  -- match a linear pattern against a term with disjoint variables:
  match :: Term -> CTerm -> Maybe (Sub CTerm)
  match (Var v) t = Just [(v,t)]
  match (Func _ pf pargs) t = case t of
    CBot           -> Nothing
    CVar _         -> Nothing
    CCons tf targs -> if pf==tf then matchCTerms pargs targs else Nothing

-- is bottom term t1 a generalization (i.e., with less information) than t2?
lessCSpecific :: CTerm -> CTerm -> Bool
lessCSpecific CBot _ = True
lessCSpecific (CVar _) _ = False
lessCSpecific (CCons c1 args1) t2 = case t2 of
  CCons c2 args2 -> c1==c2 && all (uncurry lessCSpecific) (zip args1 args2)
  _              -> False

-- show a constructor term
showCTerm :: CTerm -> String
showCTerm CBot = "_"
showCTerm (CVar i) = 'x' : show i
showCTerm (CCons f []) = f
showCTerm (CCons f args@(_:_)) =
  f ++ "(" ++ intercalate "," (map showCTerm args) ++ ")"

-- The structure of the concrete domain:
concreteDom :: ADom CTerm
concreteDom = ADom CBot (error "Cannot handle free variables") CCons
                   matchCTerms lessCSpecific showCTerm (\_ _ -> Nothing)


----------------------------------------------------------------------------
-- Abstract domain: depth-k terms

-- depth-k terms are constructor terms with cut variables:
data DTerm = DBot | DCons String [DTerm] | CutVar
 deriving (Eq, Ord)

-- pairwise matching of a list of patterns against a list of terms
matchDTerms :: [Term] -> [DTerm] -> Maybe (Sub DTerm)
matchDTerms [] [] = Just []
matchDTerms [] (_:_) = Nothing
matchDTerms (_:_) [] = Nothing
matchDTerms (x:xs) (y:ys) = combineSubst (match x y) (matchDTerms xs ys)
 where
  combineSubst Nothing   _         = Nothing
  combineSubst (Just _ ) Nothing   = Nothing
  combineSubst (Just s1) (Just s2) = Just (s1++s2)

  -- match a linear pattern against a term with disjoint variables:
  match :: Term -> DTerm -> Maybe (Sub DTerm)
  match (Var v) t = Just [(v,t)]
  match (Func _ pf pargs) t = case t of
    DBot   -> Nothing
    CutVar -> Just (map (\v->(v,CutVar)) (concatMap varsOf pargs))
    DCons tf targs -> if pf==tf then matchDTerms pargs targs else Nothing

-- apply constructor on depth-k terms
consDTerm :: Int -> String -> [DTerm] -> DTerm
consDTerm maxdepth c args = cutDTerm maxdepth (DCons c args)

-- cut a depth-k term larger than a given depth:
cutDTerm :: Int -> DTerm -> DTerm
cutDTerm _ DBot = DBot
cutDTerm _ CutVar = CutVar
cutDTerm d (DCons c args) | d==0      = CutVar
                          | otherwise = DCons c (map (cutDTerm (d-1)) args)

-- is abstract term t1 a generalization (i.e., with less information) than t2?
lessDSpecific :: DTerm -> DTerm -> Bool
lessDSpecific DBot _ = True
lessDSpecific CutVar t = t==CutVar
lessDSpecific (DCons c1 args1) t2 = case t2 of
  DCons c2 args2 -> c1==c2 && all (uncurry lessDSpecific) (zip args1 args2)
  _              -> False

-- Abstract application of a primitive operation to abstract terms.
-- The result is Nothing if this application is not defined, e.g.,
-- becuase the operation is not a primitive one.
applyPrimDTerm :: String -> [DTerm] -> Maybe DTerm
applyPrimDTerm f args
  | f `elem` allStrictOps
  = if DBot `elem` args
      then Just DBot   -- these primitive operations are strict in all arguments
      else Just CutVar -- otherwise we do not know anything
  | f `elem` fstP2StrictOps
  = if head args == DBot
      then Just DBot  -- strict in first argument
      else Just (args!!1) -- otherwise it is as most defined as second argument
  | f == "failed"
  = Just DBot -- result of failed is always undefined
  | otherwise = Nothing
 where
  -- operations strict in all arguments
  allStrictOps = ["+","-","*","mod","div","<","<=",">",">=","==","/=",
                  "=:=","show","isEmpty"]
  -- operations strict in first argument and projection on second argument
  fstP2StrictOps = ["cond","set1"]

-- show a depth-k term
showDTerm :: DTerm -> String
showDTerm DBot         = "_"
showDTerm CutVar       = "*"
showDTerm (DCons f []) = f
showDTerm (DCons f args@(x:xs))
 | f == ":" && length xs == 1 = -- format list constructor:
   showDTerm x ++ ":" ++ showDTerm (head xs)
 | take 2 f == "(," = -- format tuple constructors:
   "(" ++ intercalate "," (map showDTerm args) ++ ")"
 | otherwise =
   f ++ "(" ++ intercalate "," (map showDTerm args) ++ ")"

-- The structure of the depth-k domain (where the depth is given as an
-- argument):
depthDom :: Int -> ADom DTerm
depthDom k = ADom DBot (const CutVar) (consDTerm k) matchDTerms lessDSpecific
                  showDTerm applyPrimDTerm


------------------------------------------------------------------------------
-- Generic operations:

-- generic equality on interpretations (note that equations are kept ordered)
eqSemInt :: Eq a => SemInt a -> SemInt a -> Bool
eqSemInt = (==)

-- Generic ordered insertion of semantic equations into an interpretation.
-- The first argument is some ordering on terms (compatible with the
-- information ordering on terms). An equation is not inserted
-- if it is already there, i.e., the interpretation is managed as a set.
insertSemEq :: Ord a => SemEq a -> SemInt a -> SemInt a
insertSemEq x []     = [x]
insertSemEq x (y:ys) | x == y    = y : ys
                     | x <= y    = x : y : ys
                     | otherwise = y : insertSemEq x ys

-- Generic ordered insertion of semantic equations into an interpretation
-- where existing equations with less information are removed from the
-- interpretation. Thus, the resulting interpretation contains
-- equations with more specific abstract values.
-- First argument: ordering relation on abstract terms (used to order
--                 all equations of the interpretation)
-- Second argument: less-specific ordering on equations
updateSemEq :: Ord a => (SemEq a -> SemEq a -> Bool)
                     -> SemEq a -> SemInt a -> SemInt a
updateSemEq _              x []     = [x]
updateSemEq lessSpecificEq x (y:ys)
 | x == y             = y : ys
 | lessSpecificEq y x = updateSemEq lessSpecificEq x ys
 | lessSpecificEq x y = y : ys
 | x <= y             = x : y : ys
 | otherwise          = y : updateSemEq lessSpecificEq x ys

-- Is semantic equation e1 less specific than e2 w.r.t. some
-- call pattern? The first argument is the information ordering on terms.
-- A semantic equation is less specific iff all arguments and the result
-- of the equations are pairwise less specific.
lessSpecificEqCallPattern :: (a->a->Bool) -> SemEq a -> SemEq a -> Bool
lessSpecificEqCallPattern lessSpecific (Eq f1 args1 v1) (Eq f2 args2 v2) =
  f1==f2 && all (uncurry lessSpecific) (zip args1 args2) && lessSpecific v1 v2

-- Is semantic equation e1 less specific than e2 w.r.t. their results?
-- The first argument is the information ordering on terms.
-- A semantic equation is less specific iff all arguments are identical
-- and the result of the equations are less specific.
lessSpecificEqResult :: Eq a => (a->a->Bool) -> SemEq a -> SemEq a -> Bool
lessSpecificEqResult lessSpecific (Eq f1 args1 v1) (Eq f2 args2 v2) =
  f1==f2 && args1==args2 && lessSpecific v1 v2

------------------------------------------------------------------------------
-- Transformation on (abstract) interpretations:
-- (transformInt adom insertsem trs mains int)
-- adom      : abstract domain
-- insertsem : operation to insert new semantic equation into an interpretation
-- trs       : term rewriting system
-- mains     : main calls (start interpretation)
-- int       : interpretation to be transformed
-- result: transformed interpretation
transformInt :: Eq a => ADom a -> (SemEq a -> SemInt a -> SemInt a)
                     -> [Rule] -> SemInt a -> SemInt a -> SemInt a
transformInt (ADom abottom avar acons matchterms _ _ applyprim)
             insertsem trs mains int =
  foldr insertsem mains
        (concatMap (\ (Eq fi argsi _) ->
                     maybe (concatMap (applyRule fi argsi) (fRules fi))
                           (\ares -> [Eq fi argsi ares])
                           (applyprim fi argsi))
                   int)
 where
  fRules f = let rules = funcRules f trs
              in if null rules
                 then error ("Rules of operation '"++f++"' not found!")
                 else rules

  applyRule fi argsi (largs,rhs) =
    maybe []
          (\s -> map (Eq fi argsi) (evalInt s int rhs)
                 ++ concatMap (newSemEq s) (fSubterms rhs))
          (matchterms largs argsi)

  newSemEq s (f,args) =
    map (\iargs -> Eq f iargs abottom) (extendListMap (evalInt s int) args)

  -- abstract evaluation of a term w.r.t. a given substitution
  -- and interpretation
  evalInt sub _ (Var v) = [maybe (avar v) id (lookup v sub)]
  evalInt sub eqs (Func Cons c args) =
    map (acons c) (extendListMap (evalInt sub eqs) args)
  evalInt sub eqs (Func Def f args) =
    let evalargs = extendListMap (evalInt sub eqs) args
     in abottom :
        concatMap (\ (Eq fi argsi r) ->
              if any (\eargs->fi==f && eargs==argsi) evalargs then [r] else [])
                  eqs

-- extend list mapping on a list of elements
extendListMap :: (a -> [b]) -> [a] -> [[b]]
extendListMap _ []     = [[]]
extendListMap f (x:xs) = [ y:ys | y <- f x, ys <- extendListMap f xs]


------------------------------------------------------------------------------
-- Runs a simple fixpoint computation w.r.t. a set of abstract initial calls.
runFixpoint :: Eq a => ADom a
            -> (SemEq a -> SemInt a -> SemInt a)
            -> [Rule] -> [(String,[a])] -> Bool
            -> ([SemEq a] -> [SemEq a] -> Bool)
            -> IO (SemInt a)
runFixpoint adom insertsem rules mainacalls withprint semeq = do
  let trm = transformInt adom insertsem rules
                         (foldr insertsem [] (map main2int mainacalls))
  --printProgram rules maincalls
  if withprint then return () else putStr "Iterating:"
  garbageCollect
  pi1 <- getProcessInfos
  fpsem <- computeFixpoint withprint 0 (showSemInt adom) semeq trm []
  getProcessInfos >>= printTiming pi1
  return fpsem
 where
  -- map a main call into an abstract equation
  main2int (f,aterms) = Eq f aterms (adomBottom adom)

-- Performs an iterated fixpoint computation.
computeFixpoint :: Bool -> Int -> (a->String) -> (a->a->Bool) -> (a->a) -> a
                -> IO a
computeFixpoint withprint n prt eq f v = do
  if withprint then putStrLn (show n ++ ": " ++ prt v)
               else putStr (' ':show n) >> hFlush stdout
  let fv = f v
  if  eq v fv
   then do putStrLn $ "\nFixpoint reached after " ++ show n ++ " iterations"
           return v
   else computeFixpoint withprint (n+1) prt eq f fv


-- show a semantic interpretation w.r.t. an abstract domain
-- (containing a show function for semantic elements)
showSemInt :: ADom a -> SemInt a -> String
showSemInt adom eqs =
  let semIntLine = "{" ++ intercalate ", " (map showEq eqs) ++ "}"
   in show (length eqs) ++ " semantic equations:\n" ++
      if length semIntLine < 80
      then semIntLine
      else "{" ++ intercalate ",\n " (map showEq eqs) ++ "}"
 where
  showe = adomShow adom

  showEq (Eq f args r) =
    f ++ (if null args
          then []
          else '(' : intercalate "," (map showe args) ++ ")")
      ++ " = " ++ showe r

-- get standard main call (i.e., main(var1,...,varn)) from the rules:
getMainCall :: [Rule] -> (String,[Term])
getMainCall rules = ("main", genArgs (arityOf "main" rules))
 where
  genArgs n = map Var [1..n]

-- get standard main calls (i.e., for each function f/n and each
-- i \in [1..n] the call f(var_1,...,var_{i-1},bottom,var_{i+1},...,var_n))
-- from the rules:
genMainCalls :: ADom a -> [Rule] -> [(String,[a])]
genMainCalls (ADom abot avar _ _ _ _ _) rules =
  concatMap genStrictCalls (allFunctions rules)
 where
  genStrictCalls (f,n) = map genStrictCall [1..n]
    where genStrictCall i = (f, replace abot (i-1) (map avar [1..n]))

-- Prints a program and a list of abstract main calls.
-- The abstract domain is provided as a first argument.
printProgram :: ADom a -> [Rule] -> [(String,[a])] -> IO ()
printProgram adom rules maincalls = do
  putStrLn $ "\nRewrite rules:\n\n" ++ showTRS rules
  putStrLn $ "\nMain calls: " ++
             intercalate ", " (map showATermCall maincalls)
  putStrLn ""
 where
  showATerm = adomShow adom

  showATermCall (f,args) =
    f ++ (if null args
          then []
          else '(' : intercalate "," (map showATerm args) ++ ")")

------------------------------------------------------------------------------
-- Fixpoint computation based on working lists.

-- The current semantic equations are represented as a mapping
-- from function names into a list of equations (arguments,results)
-- together with a list of function names on which the equation depends
type WorkSemInt a = Map.Map String [([a],a,[String])]

-- Look up the semantic equations of a function in the current semantics:
fEqsOfWorkSem :: String -> WorkSemInt a -> [([a],a,[String])]
fEqsOfWorkSem f ws = maybe [] id (Map.lookup f ws)

-- Process a working list of (abstract) calls to compute an interpretation:
-- (processWorkList adom lessSpecificEq	showabs trs wl finals)
-- adom      : structure of the abstract domain
-- lessSpecificEq: less-specific ordering on equations (less-specific equations
--                 are usually removed from the abstract semantics)
-- withprint : should intermediate results be printed?
-- trs       : term rewriting system
-- wl        : working list (i.e., list of abstract calls that must be
--             analyzed and inserted in final interpretation)
-- finals    : (currently) final semantic equations together with
--             a list of function names on which they depend
-- result: final interpretation

processWorkList :: Eq a => ADom a -> (SemEq a->SemEq a->Bool) -> Bool -> [Rule]
                        -> [(String,[a])] -> WorkSemInt a -> IO (SemInt a)

processWorkList _ _ _ _ [] finals =
  -- transform the final semantic into the usual format:
  return (concatMap (\ (f,feqs) -> map (\ (args,res,_) -> Eq f args res) feqs)
                    (Map.toList finals))

processWorkList adom@(ADom abottom avar acons matchterms _ _ applyprim)
       lessSpecificEq withprint trs wl@((fc,eargs) : working) finals = do
  if withprint
   then putStr (" W" ++ show (length wl) ++
                "/F" ++ show (length (Map.toList finals)))
   else return ()
  let fcRules = let rules = funcRules fc trs
                 in if null rules
                    then error ("Rules of operation '"++fc++"' not found!")
                    else rules

      (newwss,newfeqss) = maybe (unzip (map applyRule fcRules))
                                (\ares -> ([],[[(eargs,ares,[])]]))
                                (applyprim fc eargs)

      (newws,newfeqs)   = (concat newwss, concat newfeqss)
      betterCalls       = filter isBetterCall newws
      betterEquations   = filter hasBetterResult
                                 (if null newfeqs
                                  then [(eargs,abottom,[])] -- no matching rule
                                  else newfeqs)
      bestEquations     = filter (\e-> not (any (\ei->ei/=e && leqEqDep e ei)
                                                betterEquations))
                                 betterEquations
      activatedEqs =
        if null bestEquations
        then []
        else concatMap (\ (f,feqs) -> concatMap (\ (args,_,deps) ->
                            if fc `elem` deps then [(f,args)] else []) feqs)
                       (Map.toList finals)
  --putStrLn ("WORKING:" ++
  --          showSemInt adom (map (\ (f,args)->Eq f args abottom) wl))
  --putStrLn ("FINAL:" ++ showSemInt adom
  --     (concatMap (\ (f,feqs) -> map (\ (args,res,_) -> Eq f args res) feqs)
  --                (Map.toList finals)))
  --putStrLn ("Function: " ++ fc)
  --putStrLn ("BETTER: " ++ show betterEquations)
  --putStrLn ("BEST  : " ++ show bestEquations)
  --putStrLn ("BETTER: " ++ show betterFuncs)
  --putStrLn ("ACTIVE: " ++ show activatedEqs)
  processWorkList adom lessSpecificEq withprint trs
                 (foldr insertIfBetterCall working
                        (betterCalls ++ activatedEqs))
                 (foldr insertBetterIntoRemaining finals bestEquations)
 where
  -- insert better (dependency) equations  and delete all less specific ones:
  insertBetterIntoRemaining bettereq wsem =
    let oldfceqs = fEqsOfWorkSem fc wsem
     in Map.insert fc
          (bettereq : filter (\oldeq -> not (leqEq oldeq bettereq)) oldfceqs)
          wsem

  -- insert given abstract call if there does not already exist one
  -- which is more specific than this one; if the call is inserted,
  -- all less specific calls are removed
  --insertIfBetterCall :: (String,[a]) -> [(String,[a])] -> [(String,[a])]
  insertIfBetterCall eq wlist =
    if any (leqCall eq) wlist
      then wlist
      else eq : filter (\e -> not (leqCall e eq)) wlist
   where
    leqCall (f,args) (f',args') =
      lessSpecificEq (Eq f args abottom) (Eq f' args' abottom)

  -- is a given abstract call more specific than all equations
  -- in the current final interpretation?
  --isBetterCall :: (String,[a]) -> Bool
  isBetterCall (f,args) =
    not (any (\ (args',_,_) ->
                  lessSpecificEq (Eq f args abottom) (Eq f args' abottom))
             (fEqsOfWorkSem f finals))

  -- is a given equation for function fc a better approximation than
  -- anything in the current final equations,
  -- i.e., if there does not already exist one final equation which has
  -- an identical left-hand side but a more specific result than this one
  hasBetterResult eq = not (any (leqEq eq) (fEqsOfWorkSem fc finals))

  -- compare given equation: left-hand sides and right-hand sides
  -- in leq relation on terms?
  leqEq (args,r,_) (args',r',_) =
    lessSpecificEq (Eq fc args r) (Eq fc args' r')

  -- compare given equation and their dependencies:
  -- left- and right-hand sides in leq relation on terms and dependencies
  -- subsumed?
  leqEqDep (args,r,ds) (args',r',ds') =
    lessSpecificEq (Eq fc args r) (Eq fc args' r') && all (`elem` ds') ds

  applyRule (largs,rhs) =
    maybe ([],[])
          (\s -> (concatMap (newCall s) (fSubterms rhs),
                  map (\ri -> (eargs,ri,depFuncs))
                      (evalInt s finals rhs)))
          (matchterms largs eargs)
   where
    depFuncs = funcsInTerm rhs

  newCall s (f,args) = map (\iargs->(f,iargs))
                           (extendListMap (evalInt s finals) args)

  -- abstract evaluation of a term w.r.t. a substitution and interpretation
  --evalInt :: Sub a -> WorkSemInt a -> Term -> [a]
  evalInt sub _ (Var v) = [maybe (avar v) id (lookup v sub)]
  evalInt sub eqs (Func Cons c args) =
    map (acons c) (extendListMap (evalInt sub eqs) args)
  evalInt sub eqs (Func Def f args) =
    let evalargs = extendListMap (evalInt sub eqs) args
        results = concatMap (\ (argsi,r,_) ->
                                  if any (\evargs->evargs==argsi) evalargs
                                  then [r]
                                  else [])
                            (fEqsOfWorkSem  f eqs)
     in if null results then [abottom] else results

-- run a fixpoint computation with working lists starting form a given
-- list of abstract function calls:
runFixpointWL :: Eq a => ADom a -> (SemEq a -> SemEq a -> Bool) -> [Rule]
              -> [(String,[a])] -> Bool
              -> IO (SemInt a)
runFixpointWL adom lessSpecificEq rules maincalls withprint = do
  garbageCollect
  pi1 <- getProcessInfos
  finals <- processWorkList adom lessSpecificEq withprint rules
                            maincalls Map.empty
  pi2 <- getProcessInfos
  printTiming pi1 pi2
  return finals

-- show timing w.r.t. some process infos at the start and stop point
-- of a computation:
printTiming :: [(ProcessInfo,Int)] -> [(ProcessInfo,Int)] -> IO ()
printTiming startPInfos stopPInfos = do
  putStrLn $ "Run time:            "
             ++ (showInfoDiff startPInfos stopPInfos RunTime) ++ " msec."
  putStrLn $ "Elapsed time:        "
             ++ (showInfoDiff startPInfos stopPInfos ElapsedTime) ++ " msec."
  putStrLn $ "Garbage collections: "
             ++ (showInfoDiff startPInfos stopPInfos GarbageCollections)
 where
  showInfoDiff infos1 infos2 item =
    maybe "n/a"
          (\i1 -> show (maybe 0 id (lookup item infos2) - i1))
          (lookup item infos1)


------------------------------------------------------------------------------
--- Main calls to the (abstract) interpreters:
------------------------------------------------------------------------------
-- main function to call the analyser as a saved state:
main :: IO ()
main = do
  args <- getArgs
  let (depth,max,wlist,callpat,prog) = checkArgs (1,False,True,False,"") args
  if callpat
    then callPatternAnalysis depth max wlist (dropExtension prog)
    else transformNondet depth max wlist (dropExtension prog)

mainCallError :: [String] -> _
mainCallError args = error $ unlines
  [ "Illegal arguments: " ++ unwords args
  , ""
  , "Usage: curry-ndopt [-d <k>] [-max] [-fix|-wlist] [-call] <module_name>"
  , ""
  , "Options:"
  , "<k>   : term depth (default: 1)"
  , "-max  : compute only maximal abstract elements in fixpoints"
  , "-fix  : use simple fixpoint iteration"
  , "-wlist: use working list fixpoint computation (default)"
  , "-call : compute only call patterns (and do not transform program)"
  ]

checkArgs :: (Int,Bool,Bool,Bool,String) -> [String]
          -> (Int,Bool,Bool,Bool,String)
checkArgs (depth,max,wlist,callpat,prog) args = case args of
  [] -> mainCallError []
  ("-d":ks:margs) -> let k = case readNat ks of
                               [(n,"")] -> n
                               _        -> mainCallError args
                      in checkArgs (k,max,wlist,callpat,prog) margs
  ("-max":margs) -> checkArgs (depth,True,wlist,callpat,prog) margs
  ("-wlist":margs) -> checkArgs (depth,max,True,callpat,prog) margs
  ("-fix":margs) -> checkArgs (depth,max,False,callpat,prog) margs
  ("-call" :margs) -> checkArgs (depth,max,wlist,True,prog) margs
  [mainmod] -> (depth,max,wlist,callpat,mainmod)
  _ -> mainCallError []

-- Call pattern analysis with depth-k domain.
callPatternAnalysis :: Int -> Bool -> Bool -> String -> IO ()
callPatternAnalysis termdepth keepmax withwlist modname = do
    rules <- readRules modname
    let absdom = depthDom termdepth
        maincalls = [abstractCall absdom (getMainCall rules)]
        lessSpecificEq = lessSpecificEqCallPattern lessDSpecific
    printProgram absdom rules maincalls
    let seminsertion = if keepmax then updateSemEq lessSpecificEq
                                  else insertSemEq
    fpsem <- if not withwlist
             then runFixpoint absdom seminsertion rules maincalls False eqSemInt
             else runFixpointWL absdom lessSpecificEq rules maincalls False
    putStrLn (showSemInt absdom (sort fpsem))

-- Non-determinism transformation by strictness/overlapping analysis
transformNondet :: Int -> Bool -> Bool -> String -> IO ()
transformNondet termdepth keepmax withwlist modname = do
    (FC.Prog _ imports typedecls _ _, rules) <- readFlatCurryRules modname
    let absdom = depthDom termdepth
        maincalls = genMainCalls absdom rules
        lessSpecificEq = lessSpecificEqResult lessDSpecific
    printProgram absdom rules maincalls
    let seminsertion = if keepmax then updateSemEq lessSpecificEq
                                  else insertSemEq
    fpsem <- if not withwlist
             then runFixpoint absdom seminsertion rules maincalls False eqSemInt
             else runFixpointWL absdom lessSpecificEq rules maincalls False
    putStrLn (showSemInt absdom (sort fpsem))
    let strinfos = sortFuncInfos (extractStrictness absdom fpsem)
    putStrLn ('\n' : showStrictness strinfos)
    createAnalysisDir modname
    writeFile (strictInfoFileName modname) (show strinfos)
    ndinfos <- getNondetInfos modname >>= return . sortFuncInfos
    putStrLn ("Computed non-determinism information:\n"++show ndinfos++"\n")
    writeFile (ndInfoFileName modname) (show ndinfos)
    let (transrules,numopts) = transformRules ndinfos strinfos rules
        newprog = letDropping $ unApply transrules
        newprogtxt = showTRS newprog
    putStrLn $ "Transformed program (stored in '"++modname++"_O.curry'):"
    putStrLn newprogtxt
    writeFile (modname++"_O.curry")
              (unlines (map ("import "++) (filter (/="Prelude") imports)) ++
               concatMap showDataDeclAsCurry typedecls ++ newprogtxt)
    putStrLn $ "Number of performed optimizations: " ++ show numopts
 where
  showDataDeclAsCurry fd =
    showCurryDataDecl (FC.showQNameInModule (dataModule fd)) fd

-----------------------------------------------------------------------
-- Remove all occurrences of generated apply operation in a program
unApply :: [Rule] -> [Rule]
unApply = map unApplyInRule . filter (not . applyRule)
 where
  -- does the rule defines the generated apply operation?
  applyRule (Rule f _ _) = f == "apply"

  unApplyInRule :: Rule -> Rule
  unApplyInRule (Rule f args exp) = Rule f args (unApplyInExp exp)

  unApplyInExp :: Term -> Term
  unApplyInExp (Var i) = Var i
  unApplyInExp (Func Cons c args) = Func Cons c (map unApplyInExp args)
  unApplyInExp (Func Def f args) =
    if f=="apply" && length args == 2
    then Func Def "$" args
    else Func Def f (map unApplyInExp args)
  
-----------------------------------------------------------------------
-- Optimize program w.r.t. non-determinism and strictness information
-- Returns new rules and number of optimization transformations performed
transformRules :: [(String,Bool)] -> [(String,[Int])] -> [Rule] -> ([Rule],Int)
transformRules ndinfos strinfos rules =
  let (newrules,numopts) = unzip $
        map (\ (Rule f args exp) ->
              let (newrhs,_,numopt) = transformExp ndinfos strinfos exp
               in (Rule f args newrhs, numopt))
            rules
   in (newrules, foldr (+) 0 numopts)

-- Transform expression w.r.t. non-determinism and strict information.
-- Returns:
-- * transformed expression
-- * is the expression non-deterministic?
-- * number of optimization transformations applied
transformExp :: [(String,Bool)] -> [(String,[Int])] -> Term -> (Term,Bool,Int)
transformExp _ _ (Var i) = (Var i, False, 0)
transformExp ndinfos strinfos (Func Cons c args) =
  let (targs,ndargs,numopts) = unzip3 (map (transformExp ndinfos strinfos) args)
   in (Func Cons c targs, or ndargs, foldr (+) 0 numopts)
transformExp ndinfos strinfos (Func Def f args) =
  let (targs,ndargs,numopts) = unzip3 (map (transformExp ndinfos strinfos) args)
      isndexp = (maybe False id (lookup f ndinfos)) || or ndargs
      fsargs  = maybe [] id (lookup f strinfos)
      argopts = foldr (+) 0 numopts
      strictapplies = foldr (\si sargs -> replace (ndargs!!(si-1)) (si-1) sargs)
                            (take (length args) (repeat False))
                            fsargs
   in if null fsargs || not (or ndargs) || not (or strictapplies)
      then (Func Def f targs, isndexp, argopts)
      else (strictApply (Func Def f []) strictapplies targs, isndexp, argopts+1)
 where
  strictApply exp [] [] = exp
  strictApply exp (str:strs) (x:xs) =
    strictApply (Func Def (if str then "$!" else "$") [exp,x]) strs xs

-- extract list of strict arguments for each function from least fixpoint
extractStrictness :: Eq a => ADom a -> SemInt a -> [(String,[Int])]
extractStrictness adom aint = map checkStrictArgs allfuncs
 where
  abot = adomBottom adom

  allfuncs = nub (map (\ (Eq f args _) -> (f,length args)) aint)

  checkStrictArgs (f,n) = (f,concatMap checkStrictArg [1..n])
   where
     checkStrictArg i =
       let argibotEqs = filter (\ (Eq g args _) -> f==g && args!!(i-1) == abot)
                               aint
        in if not (null argibotEqs) && all (\ (Eq _ _ r) -> r==abot) argibotEqs
           then [i]
           else []

-- Sort list of operation information by function names:
sortFuncInfos :: [(String,a)] -> [(String,a)]
sortFuncInfos = sortBy (\i1 i2 -> fst i1 <= fst i2)

-- Show strictness information of all functions a little bit formatted:
showStrictness :: [(String,[Int])] -> String
showStrictness info =
  "Computed strictness information:\n"++
  unlines (map (\ (f,sargs) -> if null sargs
                                 then f ++ " not strict"
                                 else f ++ " strict at " ++
                                      intercalate "," (map show sargs))
               info)

dataModule :: FC.TypeDecl -> String
dataModule (FC.Type tname _ _ _) = fst tname
dataModule (FC.TypeSyn tname _ _ _) = fst tname
dataModule (FC.TypeNew tname _ _ _) = fst tname

------------------------------------------------------------------------------
-- operations for benchmarking

-- define where to look for the benchmark programs:
prog2DirFile :: String -> String
prog2DirFile = ("benchmarks_callpattern/" ++)

-- benchmark different interpretation methods:
bench :: Int -> String -> IO ()
bench k file = do
  putStrLn (take 70 (repeat '='))
  rules <- readRules (prog2DirFile file)
  let absdom = depthDom k
      maincalls = [abstractCall absdom (getMainCall rules)]
  printProgram absdom rules maincalls
  putStrLn $ "\nTotal number of rules: " ++ show (length rules)
  putStrLn "\nRunning simple call analysis..."
  semsimple <- runFixpoint absdom insertSemEq
                           rules maincalls False eqSemInt
  putStrLn (showSemInt absdom semsimple)
  putStrLn "\nRunning call analysis with ordered insertion..."
  semord <- runFixpoint absdom (updateSemEq
                                  (lessSpecificEqCallPattern lessDSpecific))
                        rules maincalls False eqSemInt
  putStrLn (showSemInt absdom semord)
  putStrLn "\nRunning call analysis with working lists..."
  semwl <- runFixpointWL absdom (lessSpecificEqCallPattern lessDSpecific)                                 rules maincalls False >>=
           return . sort
  if semord==semwl
    then return ()
    else putStrLn (showSemInt absdom semwl)

-- run the benchmarks of the examples directory:
runBench :: IO ()
runBench = do
  bench 1 "addadd"
  bench 2 "addlast"
  bench 1 "bertf0"
  bench 1 "bertconc"
  bench 1 "doublecoin"
  bench 1 "family"
  bench 2 "halfdouble"
  bench 1 "head"
  bench 1 "lastapp"
  bench 8 "readfile"
  bench 2 "mapadddouble"
  bench 1 "risers"
  bench 1 "tails"

------------------------------------------------------------------------------
