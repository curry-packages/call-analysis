-- Some useful operations on FlatCurry programs:

module MyFlatCurryGoodies where

import FlatCurry.Types

moduleName (Prog mod _ _ _ _) = mod

importsOfProg :: Prog -> [String]
importsOfProg (Prog _ imps _ _ _) = imps

typesOfProg (Prog _ _ types _ _) = types

funcsOfProg (Prog _ _ _ funcs _) = funcs

dataModule (Type tname _ _ _) = fst tname
dataModule (TypeSyn tname _ _ _) = fst tname


funcName (Func fname _ _ _ _) = fname

unqualifiedName (Func fname _ _ _ _) = snd fname

funcModule (Func fname _ _ _ _) = fst fname

isPublic :: FuncDecl -> Bool
isPublic (Func _ _ vis _ _) = vis==Public
