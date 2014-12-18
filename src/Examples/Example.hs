{-# LANGUAGE OverloadedStrings #-}

module Examples.Example where

import Data.Monoid
import Data.Sansible
import AnsibleModules.Apt

import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  let task = Task
               { taskName = "Install Haskell"
               , taskTags = []
               , moduleCall = compile (defaultApt "ghc")
               }

      playbook = Playbook
                   { pbHosts    = HostPattern "localhost"
                   , pbSudo     = Just True
                   , pbTags     = mempty
                   , pbSudoUser = Nothing
                   , pbTasks    = [task]
                   }

  BS.putStrLn $ encode [playbook]
