module AnsibleModules.Copy where

import qualified Data.Sansible as S
import Data.Sansible.Playbook

import qualified Data.Text     as T
import qualified Data.Aeson.TH as A

data Copy = Copy
          { backup        :: Maybe Bool
          , content       :: Maybe T.Text
          , dest          :: FilePath
          , directoryMode :: Maybe Bool
          , force         :: Maybe Bool
          , group         :: Maybe S.Group
          , owner         :: Maybe S.User
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
         -> S.File
         -> CompiledModuleCall
copyFile s (S.File p m o g) =
  compile (defaultCopy p)
    { mode   = Just m
    , src    = Just s
    , group  = Just g
    , owner  = Just o
    }

copyText :: T.Text
         -> S.File
         -> CompiledModuleCall
copyText c (S.File p m o g)  =
  compile (defaultCopy p)
    { content = Just c
    , mode    = Just m
    , group   = Just g
    , owner   = Just o
    }

$(A.deriveToJSON S.encodingOptions ''Copy)
