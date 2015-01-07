module AnsibleModules.Git where

import Data.Sansible
import Data.Sansible.Playbook
import Network.URI

import qualified Data.Aeson.TH as A
import qualified Data.Text     as T

data Git = Git
         { acceptHostKey   :: Maybe Bool
         , bare            :: Maybe Bool
         , depth           :: Maybe Int
         , dest            :: FilePath
         , executable      :: Maybe FilePath
         , force           :: Maybe Bool
         , keyFile         :: Maybe FilePath
         , recursive       :: Maybe Bool
         , reference       :: Maybe Bool
         , remote          :: Maybe T.Text
         , repo            :: URI
         , ssh_opts        :: Maybe T.Text
         , trackSubmodules :: Maybe Bool
         , update          :: Maybe Bool
         , version         :: Maybe T.Text
         }

$(A.deriveToJSON encodingOptions ''Git)

instance ModuleCall Git where
  moduleLabel _ = "git"

defaultGit :: URI -> FilePath -> Git
defaultGit r d = Git
               { acceptHostKey   = Nothing
               , bare            = Nothing
               , depth           = Nothing
               , dest            = d
               , executable      = Nothing
               , force           = Nothing
               , keyFile         = Nothing
               , recursive       = Nothing
               , reference       = Nothing
               , remote          = Nothing
               , repo            = r
               , ssh_opts        = Nothing
               , trackSubmodules = Nothing
               , update          = Nothing
               , version         = Nothing
               }

simpleGit :: URI -> FilePath -> CompiledModuleCall
simpleGit r d = compile $ defaultGit r d
