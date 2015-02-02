module AnsibleModules.Pip where

import Data.Monoid
import Data.Sansible.Playbook
import Data.Sansible

import qualified Data.Text as T
import qualified Data.Aeson.TH as A

data State       = Present | Absent | Latest
$(A.deriveToJSON encodingOptions ''State)

type PackageName = T.Text
type Version     = T.Text

data Pip = Pip
         { name :: Maybe PackageName
         , version :: Maybe Version
         , requirements :: Maybe FilePath
         , virtualenv :: Maybe FilePath
         , virtualenvSitePackages :: Maybe Bool
         , virtualenvCommand :: Maybe FilePath
         , state :: Maybe State
         , extraArgs :: Maybe T.Text
         , chdir :: Maybe FilePath
         , executable :: Maybe FilePath
         }
$(A.deriveToJSON encodingOptions ''Pip)

instance ModuleCall Pip where
  moduleLabel _ = "pip"

defaultPip :: Pip
defaultPip = Pip
         { name = Nothing
         , version = Nothing
         , requirements = Nothing
         , virtualenv = Nothing
         , virtualenvSitePackages = Nothing
         , virtualenvCommand = Nothing
         , state = Nothing
         , extraArgs = Nothing
         , chdir = Nothing
         , executable = Nothing
         }


simplePipInstall :: T.Text -> Maybe Version -> Task
simplePipInstall pkg v = task ("Install " <> pkg <> maybe "" (", version " <>) v)
                              (compile $ defaultPip { name = Just pkg
                                                    , version = v
                                                    }
                              )
