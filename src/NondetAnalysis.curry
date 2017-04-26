import Nondeterminism
import FlatCurry.Types
import FlatCurry.Read
import FlatCurry.Goodies

getNondetInfos :: String -> IO [(String,Bool)]
getNondetInfos fname = do
  progs <- readFlatCurryWithImports fname
  let allfuns = analyseSetValued (concatMap progFuncs progs)
      mainfnames = map funcName (progFuncs (head progs))
  return (map (\ (qn,i) -> (snd qn,i))
              (filter (\ (qn,_) -> qn `elem` mainfnames) allfuns))


