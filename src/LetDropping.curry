-- Replace calls to auxiliary let functions (introduced
-- by OrCaseLifter) by let expressions

import TRS
import Read

letDropping :: [Rule] -> [Rule]
letDropping rules = filter (not . letRule) (map (insertLetInRule rules) rules)
 where
  -- does the rule defines an auxiliary let operation?
  letRule (Rule f _ _) = take 4 (snd (break (=='#') f)) == "#LET"


insertLetInRule :: [Rule] -> Rule -> Rule
insertLetInRule rules (Rule f args exp) =
  Rule f args (insertLetInExp rules exp)

insertLetInExp :: [Rule] -> Term -> Term
insertLetInExp _ (Var i) = Var i
insertLetInExp rls (Func Cons c args) =
  Func Cons c (map (insertLetInExp rls) args)
insertLetInExp rls (Func Def f args) =
  let (_,hashp) = break (=='#') f
   in if null hashp || take 4 hashp /= "#LET"
      then Func Def f (map (insertLetInExp rls) args)
      else let letrules = funcRules f rls
            in if length letrules /= 1
               then error ("LetDropping: incorrect rules for "++f)
               else let (largs,lexp) = head letrules
                        freenums = readNat (fst (break (=='_') (drop 4 hashp)))
                     in replaceLetCall f args freenums
                                       (insertLetInRule rls (Rule f largs lexp))

replaceLetCall f args freenums (Rule _ largs lexp) =
  if length args == length largs
  then Func Def "LET"
            (map (\ (v,e) -> Func Cons "(,)" [v,e])
                 (drop freenums (zip largs args))
             ++ [lexp])
  else if null args
       then Func Def "LAMBDA" (largs++[lexp])
       else Func Def f args
 where
  exps2tuple exps =
    if length exps == 1
    then head exps
    else Func Cons ("("++take (length exps - 1) (repeat ',')++")") exps
