{-# LANGUAGE OverloadedStrings #-}

module Examples.Example where

import Data.Monoid
import Data.Sansible

import AnsibleModules.Apt
import AnsibleModules.File

import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  let install   = Task "Install Haskell"
                       (aptInstall "ghc")
                       ["install", "haskell"]

      createFoo = task "Create foo dir"
                       (createDir "user" "group" "755" "/tmp/foo")

      playbook = Playbook
                   { pbHosts    = "localhost"
                   , pbSudo     = Just True
                   , pbTags     = mempty
                   , pbSudoUser = Nothing
                   , pbTasks    = [install, createFoo]
                   }

  BS.putStrLn $ encode [playbook]
