module AnsibleModules.File where

import qualified Data.Sansible as S
import Data.Sansible.Playbook

import qualified Data.Text     as T
import qualified Data.Aeson.TH as A

data FileState = ChoiceFile
               | Link
               | Directory
               | Hard
               | Touch
               | Absent
$(A.deriveToJSON S.encodingOptions ''FileState)

data File = File
          { follow  :: Maybe Bool
          , force   :: Maybe Bool
          , group   :: Maybe S.Group
          , mode    :: Maybe T.Text
          , owner   :: Maybe S.User
          , path    :: FilePath
          , recurse :: Maybe Bool
          , selevel :: Maybe T.Text
          , serole  :: Maybe T.Text
          , setype  :: Maybe T.Text
          , seuser  :: Maybe T.Text
          , src     :: Maybe FilePath
          , state   :: Maybe FileState
          }

instance ModuleCall File where
  moduleLabel _ = "file"

defaultFile :: FilePath -> File
defaultFile p = File
              { follow  = Nothing
              , force   = Nothing
              , group   = Nothing
              , owner   = Nothing
              , mode    = Nothing
              , path    = p
              , recurse = Nothing
              , selevel = Nothing
              , serole  = Nothing
              , setype  = Nothing
              , seuser  = Nothing
              , src     = Nothing
              , state   = Nothing
              }

createSymlink :: FilePath
              -> FilePath
              -> CompiledModuleCall
createSymlink s d = compile $ (defaultFile d) { src = Just s }

createDir :: S.Dir
          -> CompiledModuleCall
createDir (S.Dir p m o g) = compile $ (defaultFile p)
                          { group = Just g
                          , owner = Just o
                          , mode  = Just m
                          , state = Just Directory
                          }

$(A.deriveToJSON S.encodingOptions ''File)
