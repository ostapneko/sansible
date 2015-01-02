module AnsibleModules.Copy where

import Data.Sansible
import Data.Sansible.Playbook

import qualified Data.Text     as T
import qualified Data.Aeson.TH as A

data Copy = Copy
          { backup        :: Maybe Bool
          , content       :: Maybe T.Text
          , dest          :: FilePath
          , directoryMode :: Maybe Bool
          , force         :: Maybe Bool
          , group         :: Maybe Group
          , owner         :: Maybe User
          , mode          :: Maybe T.Text
          , src           :: Maybe FilePath
          }

instance ModuleCall Copy where
  moduleLabel _ = "copy"

defaultCopy :: FilePath -> Copy
defaultCopy dest' =
  Copy { backup        = Nothing
       , dest          = dest'
       , src           = Nothing
       , content       = Nothing
       , directoryMode = Nothing
       , force         = Nothing
       , group         = Nothing
       , owner         = Nothing
       , mode          = Nothing
       }

copyFile :: FilePath
         -> FilePath
         -> User
         -> Group
         -> T.Text -- ^ The File permission
         -> CompiledModuleCall
copyFile s d o g m  =
  compile (defaultCopy d)
    { mode   = Just m
    , src    = Just s
    , group  = Just g
    , owner  = Just o
    }

copyText :: T.Text
         -> FilePath
         -> User
         -> Group
         -> T.Text -- ^ The File permission
         -> CompiledModuleCall
copyText c d o g m  =
  compile (defaultCopy d)
    { content = Just c
    , mode    = Just m
    , group   = Just g
    , owner   = Just o
    }


$(A.deriveToJSON encodingOptions ''Copy)
