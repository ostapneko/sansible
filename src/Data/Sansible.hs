module Data.Sansible where

import Data.Maybe
import Data.Monoid
import Data.Set
import Data.Yaml ( (.=) )
import Data.Char

import qualified Data.Aeson.TH as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Yaml as Y
import qualified Data.List as L

newtype HostPattern = HostPattern String deriving (Show, Y.ToJSON)
newtype User = User String deriving (Show, Y.ToJSON)
newtype Tag  = Tag String deriving (Show, Y.ToJSON, Ord, Eq)

data CompiledModuleCall = CompiledModuleCall
                        { moduleName         :: T.Text
                        , moduleArgs         :: Y.Value
                        , moduleCallFreeForm :: Maybe T.Text
                        } deriving Show

class ModuleCall m where
  compile :: m -> CompiledModuleCall

data Task = Task
  { taskName :: T.Text
  , moduleCall :: CompiledModuleCall
  , taskTags :: [Tag]
  } deriving (Show)

data Playbook = Playbook
  { pbHosts     :: HostPattern
  , pbSudo      :: Maybe Bool
  , pbSudoUser  :: Maybe User
  , pbTasks     :: [Task]
  , pbTags      :: Set Tag
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
snakeCase (c : cs) = if isLower c
                     then c : snakeCase cs
                     else '_' : toLower c : snakeCase cs

encodingOptions :: A.Options
encodingOptions = A.defaultOptions
                { A.fieldLabelModifier = snakeCase
                , A.omitNothingFields = True
                }

encode :: [Playbook] -> BS.ByteString
encode pbs = "---\n" <> Y.encode pbs
