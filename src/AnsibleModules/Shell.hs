module AnsibleModules.Shell where

import Data.Sansible
import Data.Sansible.Playbook

import qualified Data.Aeson.TH as A

import qualified Data.Text as T

data Shell = Shell
           { chdir      :: Maybe FilePath
           , creates    :: Maybe FilePath
           , executable :: Maybe FilePath
           , removes    :: Maybe FilePath
           }

$(A.deriveToJSON encodingOptions ''Shell)


defaultShell :: Shell
defaultShell = Shell
               { chdir      = Nothing
               , creates    = Nothing
               , executable = Nothing
               , removes    = Nothing
               }

instance ModuleCall Shell where
  moduleLabel _ = "shell"

simpleShell :: T.Text -> Maybe FilePath -> CompiledModuleCall
simpleShell cmd mCreates =
    mc { moduleCallFreeForm = Just cmd }
  where
    mc = compile $ defaultShell { creates = mCreates }
