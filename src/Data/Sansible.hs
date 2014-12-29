{-# LANGUAGE FlexibleInstances #-}

module Data.Sansible
  ( module Data.Sansible
  , module Data.Sansible.Common
  , module Data.Sansible.Inventory
  ) where

import Data.Maybe
import Data.Monoid
import Data.Set
import Data.Yaml ( (.=) )
import Data.Char

import qualified Data.Aeson            as A
import qualified Data.Aeson.TH         as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.List             as L
import qualified Data.Text             as T
import qualified Data.Yaml             as Y

import Data.Sansible.Common
import Data.Sansible.Inventory

data CompiledModuleCall = CompiledModuleCall
                        { moduleName         :: T.Text
                        , moduleArgs         :: Y.Value
                        , moduleCallFreeForm :: Maybe T.Text
                        } deriving Show

class (A.ToJSON m) => ModuleCall m where
  moduleLabel :: m -> T.Text
  compile :: m -> CompiledModuleCall
  compile m = CompiledModuleCall (moduleLabel m) (A.toJSON m) Nothing

data Task = Task
  { taskName :: T.Text
  , moduleCall :: CompiledModuleCall
  , taskTags :: [Tag]
  } deriving (Show)

instance Y.ToJSON Task where
  toJSON t =
    let (CompiledModuleCall modName args ff) = moduleCall t
    in  Y.object
          [ "name"  .= taskName t
          , "tags"  .= taskTags t
          , modName .= fromMaybe "" ff
          , "args"  .= args
          ]

-- ^ shortcut to create a task without tags
task :: T.Text -> CompiledModuleCall -> Task
task n m = Task n m []

data Playbook = Playbook
  { pbHosts     :: HostPattern
  , pbSudo      :: Maybe Bool
  , pbSudoUser  :: Maybe User
  , pbTasks     :: [Task]
  , pbTags      :: Set Tag
  } deriving (Show)

instance Y.ToJSON Playbook where
  toJSON p =
    Y.object $ L.filter ((/= Y.Null) . snd)
      [ "hosts"     .= pbHosts p
      , "tags"      .= pbTags p
      , "tasks"     .= pbTasks p
      , "sudo"      .= pbSudo p
      , "sudo_user" .= pbSudoUser p
      ]

snakeCase :: String -> String
snakeCase [] = []
snakeCase (c : cs) = if isUpper c
                     then '_' : toLower c : snakeCase cs
                     else c : snakeCase cs

encodingOptions :: A.Options
encodingOptions = A.defaultOptions
                { A.fieldLabelModifier = snakeCase
                , A.omitNothingFields = True
                , A.constructorTagModifier = stripChoice
                }

encode :: [Playbook] -> BS.ByteString
encode pbs = "---\n" <> Y.encode pbs

stripChoice :: String -> String
stripChoice str =
  let lower = snakeCase str
  in fromMaybe lower (L.stripPrefix "choice_" lower)
