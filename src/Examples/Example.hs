{-# LANGUAGE OverloadedStrings #-}

module Examples.Example where

import Data.Maybe
import Data.Monoid
import Data.Sansible
import Data.Sansible.Inventory
import Data.Sansible.Playbook
import qualified Data.Set                as S

import qualified Data.Sansible.Inventory as I
import qualified Data.Sansible.Playbook  as P

import qualified AnsibleModules.Apt     as MA
import qualified AnsibleModules.File    as MF
import qualified AnsibleModules.GetUrl  as MG
import qualified AnsibleModules.User    as MU
import qualified AnsibleModules.Service as MS
import qualified AnsibleModules.Git     as MG

import Network.URI

import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  let dir      = Dir "/var/foo/bar" "777" "johndoe" "wheel"
      install  = Task "Install Haskell"
                       (MA.aptInstall "ghc")
                       (S.fromList ["install", "haskell"])
                       (Just (User "johndoe"))
                       (Just 5)
                       Nothing

      createFoo = task "Create foo dir" $ MF.createDir dir

      uri             = fromMaybe (error "Could not parse URI") (parseAbsoluteURI "http://www.example.com/file.zip")
      downloadExample = task "Download file" (MG.download uri "/tmp/file.zip")
      createUser      = task "Create user Bar" $ compile (MU.defaultUser (User "Bar")) { MU.groups = Just (S.fromList [Group "bar", Group "foo"]) }
      startService    = tag "service" $ task "Start service foo" $ MS.simpleService "foo" MS.Started
      cloneRepo       = task "Clone example repo " $ MG.simpleGit (fromJust . parseURI $ "https://github.com/afiore/jenkins-tty.git" ) "/path/to/repo"

      playbook = Playbook
                   { pbHosts    = "localhost"
                   , pbSudo     = Just True
                   , pbTags     = mempty
                   , pbSudoUser = Nothing
                   , pbTasks    = [install, createFoo, downloadExample, createUser, startService, cloneRepo]
                   }
      system = (simpleSystem "localhost") { ansibleSshUser = Just "vagrant" }
      inventoryGroup = InventoryGroup (Just "local") [system]

  BS.putStrLn "# Playbook"
  BS.putStrLn $ P.encode [playbook]

  BS.putStrLn "# Inventory"
  BS.putStrLn $ I.encode [inventoryGroup]
