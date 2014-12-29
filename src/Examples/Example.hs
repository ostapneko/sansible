{-# LANGUAGE OverloadedStrings #-}

module Examples.Example where

import Data.Maybe
import Data.Monoid
import Data.Sansible.Inventory
import Data.Sansible.Playbook

import qualified Data.Sansible.Inventory as I
import qualified Data.Sansible.Playbook  as P

import AnsibleModules.Apt
import AnsibleModules.File
import AnsibleModules.GetUrl

import Network.URI

import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  let install   = Task "Install Haskell"
                       (aptInstall "ghc")
                       ["install", "haskell"]

      createFoo = task "Create foo dir"
                       (createDir "user" "group" "755" "/tmp/foo")

      uri = fromMaybe (error "Could not parse URI") (parseAbsoluteURI "http://www.example.com/file.zip")
      downloadExample = task "Download file" (download uri "/tmp/file.zip")

      playbook = Playbook
                   { pbHosts    = "localhost"
                   , pbSudo     = Just True
                   , pbTags     = mempty
                   , pbSudoUser = Nothing
                   , pbTasks    = [install, createFoo, downloadExample]
                   }
      system = (simpleSystem "localhost") { ansibleSshUser = Just "vagrant" }
      inventoryGroup = InventoryGroup (Just "local") [system]

  BS.putStrLn "# Playbook"
  BS.putStrLn $ P.encode [playbook]

  BS.putStrLn "# Inventory"
  BS.putStrLn $ I.encode [inventoryGroup]
