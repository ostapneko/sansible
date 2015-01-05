module AnsibleModules.File where

import Data.Sansible
import Data.Sansible.Playbook

import qualified Data.Text     as T
import qualified Data.Aeson.TH as A

data FileState = ChoiceFile
               | Link
               | Directory
               | Hard
               | Touch
               | Absent
$(A.deriveToJSON encodingOptions ''FileState)

data File = File
          { follow  :: Maybe Bool
          , force   :: Maybe Bool
          , group   :: Maybe Group
          , mode    :: Maybe T.Text
          , owner   :: Maybe User
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

createFile :: User
           -> Group
           -> T.Text -- ^ The file permission
           -> FilePath
           -> FilePath
           -> CompiledModuleCall
createFile u g m src' dest =  compile $ (defaultFile dest)
                           { group = Just g
                           , owner = Just u
                           , mode  = Just m
                           , src   = Just src'
                           , state = Just ChoiceFile
                           }

createSymlink :: FilePath
              -> FilePath
              -> CompiledModuleCall
createSymlink s d = compile $ (defaultFile d) { src   = Just s
                                              , state = Just Link
                                              }

createDir :: User
          -> Group
          -> T.Text -- ^ The file permission
          -> FilePath
          -> CompiledModuleCall
createDir u g m p = compile $ (defaultFile p)
                  { group = Just g
                  , owner = Just u
                  , mode  = Just m
                  , state = Just Directory
                  }

$(A.deriveToJSON encodingOptions ''File)
