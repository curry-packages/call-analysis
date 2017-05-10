------------------------------------------------------------------------------
-- Benchmarking non-determinism optimization
------------------------------------------------------------------------------

import System
import IO
import IOExts
import List

------------------------------------------------------------------------------
-- Auxiliary operations for benchmarking

-- define where to look for the benchmark programs:
prog2BenchFile p = "./"++p

-- kics2 system
kics2Exec = "kics2"

-- mcc compiler
mccExec = "/opt/mcc/bin/cyc"

-- nd optimizer executable
ndoptExec = "curry-ndopt"

-- ViaLOIS system
vialoisExec = "/net/medoc/home/mh/vialois/vialois"

evalCmd :: String -> IO String
evalCmd cmd =do h <- connectToCommand cmd 
                s <- hGetContents h 
                hClose h
                return s

isUbuntu :: IO Bool
isUbuntu = do
  bsid <- evalCmd "lsb_release -i"
  return ("Ubuntu" `isInfixOf` bsid)

-- Execute shell command with execution time printing:
execWithTimeCommand cmd = do
  timecmd <- getTimeCmd
  system $ timecmd ++ " " ++ cmd

-- Compute the time command which depends on the OS:
getTimeCmd :: IO String
getTimeCmd = do
  isubuntu <- isUbuntu
  return $ if isubuntu
           then "time --format=\"BENCHMARKTIME=%U\""
           else -- for Debian-PCs:
                "export TIMEFORMAT=\"BENCHMARKTIME=%2U\" && time"

------------------------------------------------------------------------------
-- KiCS2 benchmarking of original and optimized programs

-- benchmark a program with a given expression
runProgExp quiet prog exp =
  system $ kics2Exec++" :set v0 :l "++prog++" :eval \""++exp++"\" :q"++
           (if quiet then " > /dev/null" else "")

-- benchmark a program/expression without/with optimization:
runBench quiet prog exp = do
  putStrLn (take 70 (repeat '='))
  putStrLn $ "Program: "++prog++" / expression: "++exp
  runProgExp quiet prog exp
  system $ ndoptExec++" "++prog++(if quiet then " > /dev/null" else "")
  runProgExp quiet (prog++"_O") exp

-- run the benchmarks of the examples directory:
runAllBench quiet = do
  runBench quiet "last" "last2 [1..10000]"
  runBench quiet "last" "last6 [1..10000]"
  runBench quiet "dupnd" "addNum2 2000"
  runBench quiet "dupnd" "addNum5 2000"
  runBench quiet "dupnd" "addPair 2000"
  runBench quiet "dupnd" "addTriple 2000"
  runBench quiet "half" "half2 2000"
  runBench quiet "half" "half5 2000"
  runBench quiet "duplist" "dupList2 ([1..1000]++[1..1000])"
  runBench quiet "duplist" "dupList5 ([1..1000]++[1..1000])"
  runBench quiet "select" "select [1..100]"
  runBench quiet "queens" "queens 8"
  runBench quiet "permsort" "psort [14,13..1]"
  runBench quiet "even" "even 2000"
  done

------------------------------------------------------------------------------
-- vialois benchmarking of original programs
-- benchmark a program with a given expression
runProgExpViaLOIS quiet prog exp = do
  let quietSh = if quiet then " > /dev/null" else ""
  -- create new program with main operation
  system $ "cp "++prog2BenchFile (prog++".curry")++" Main.curry"++quietSh
  system $ "echo \"main = "++exp++"\" >> Main.curry"
  system $ vialoisExec++" --opt Main.curry"++quietSh
  -- execute compile program
  execWithTimeCommand ("./Main.ocamlopt"++quietSh)

-- benchmark a program/expression without/with optimization:
runBenchViaLOIS quiet prog exp = do
  putStrLn (take 70 (repeat '='))
  putStrLn $ "Program: "++prog++" / expression: "++exp
  runProgExpViaLOIS quiet prog exp

-- run the benchmarks of the examples directory:
runAllBenchViaLOIS quiet = do
  --runBenchViaLOIS quiet "last" "last2 [1..10000]"
  --runBenchViaLOIS quiet "last" "last6 [1..10000]"
  runBenchViaLOIS quiet "dupnd" "addNum2 2000"
  runBenchViaLOIS quiet "dupnd" "addNum5 2000"
  runBenchViaLOIS quiet "dupnd" "addPair 2000"
  runBenchViaLOIS quiet "dupnd" "addTriple 2000"
  runBenchViaLOIS quiet "half" "half2 2000"
  runBenchViaLOIS quiet "half" "half5 2000"
  --runBenchViaLOIS quiet "duplist" "dupList2 ([1..1000]++[1..1000])"
  --runBenchViaLOIS quiet "duplist" "dupList5 ([1..1000]++[1..1000])"
  runBenchViaLOIS quiet "select" "select [1..100]"
  runBenchViaLOIS quiet "permsort" "psort [14,13..1]"
  runBenchViaLOIS quiet "even" "even 2000"
  done

------------------------------------------------------------------------------
-- MCC benchmarking of original programs
-- benchmark a program with a given expression
runProgExpMCC quiet prog exp = do
  let quietSh = if quiet then " > /dev/null" else ""
  -- compile program with main operation
  system $ mccExec++" -e\""++exp++"\" "++prog++".curry"++quietSh
  -- execute compiled program
  timeCmd <- getTimeCmd
  system ("echo a | "++timeCmd++" ./a.out +RTS -h512m -RTS"++quietSh)

-- benchmark a program/expression without/with optimization:
runBenchMCC quiet prog exp = do
  putStrLn (take 70 (repeat '='))
  putStrLn $ "Program: "++prog++" / expression: "++exp
  runProgExpMCC quiet prog exp

-- run the benchmarks of the examples directory:
runAllBenchMCC quiet = do
  runBenchMCC quiet "last" "last2 [1..10000]"
  runBenchMCC quiet "last" "last6 [1..10000]"
  runBenchMCC quiet "dupnd" "addNum2 2000"
  runBenchMCC quiet "dupnd" "addNum5 2000"
  runBenchMCC quiet "dupnd" "addPair 2000"
  runBenchMCC quiet "dupnd" "addTriple 2000"
  runBenchMCC quiet "half" "half2 2000"
  runBenchMCC quiet "half" "half5 2000"
  runBenchMCC quiet "duplist" "dupList2 ([1..1000]++[1..1000])"
  runBenchMCC quiet "duplist" "dupList5 ([1..1000]++[1..1000])"
  runBenchMCC quiet "select" "select [1..100]"
  runBenchMCC quiet "permsort" "psort [14,13..1]"
  runBenchMCC quiet "even" "even 2000"
  done

------------------------------------------------------------------------------

main = do
 runAllBench True
 runAllBenchViaLOIS True
 --runAllBenchMCC True
