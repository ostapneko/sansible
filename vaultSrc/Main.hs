module Main where

import Control.Applicative
import Control.Monad
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit

import Encryption

passFileOpt :: OptDescr FilePath
passFileOpt = Option ['p'] ["password-file"] (ReqArg id "FILE") "password file"

usage :: String -> String
usage prgName = "Usage: " ++ prgName ++ " [options] [encrypt|decrypt]"

main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute [passFileOpt] args of
    (_, _, errs@(_ : _))          -> putStrLn (unlines errs) >> exitFailure
    ([passFile], ["encrypt"], []) -> checkPass passFile      >> encrypt passFile
    ([passFile], ["decrypt"], []) -> checkPass passFile      >> decrypt passFile
    _                             -> do
      prgName <- getExecutablePath
      putStrLn $ usageInfo (usage prgName) [passFileOpt]
      exitFailure

encrypt :: FilePath -> IO ()
encrypt passFile = do
  decDir <- getDecDir
  checkDecDir <- doesDirectoryExist decDir
  unless checkDecDir $ putStrLn ("Could not find the directory " ++ decDir ++ "; Aborting.") >> exitFailure
  putStrLn "Encrypting..."
  encryptAll passFile

decrypt :: FilePath -> IO ()
decrypt passFile = do
  encDir <- getEncDir
  checkEncDir <- doesDirectoryExist encDir
  unless checkEncDir $ putStrLn ("Could not find the directory " ++ encDir ++ "; Aborting.") >> exitFailure
  putStrLn "Decrypting..."
  decryptAll passFile

checkPass :: FilePath -> IO ()
checkPass passFile = do
  exists <- doesFileExist passFile
  unless exists $ putStrLn $ "The password file " ++ passFile ++ " does not exist!"
