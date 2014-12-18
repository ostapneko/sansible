module Data.Sansible where

import Control.Applicative
import Data.Maybe
import Data.Monoid
import Data.Set
import Data.Yaml ( (.=) )

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Yaml as Y


newtype HostPattern = HostPattern String deriving (Show, Y.ToJSON)
newtype User = User String deriving (Show, Y.ToJSON)
newtype Tag  = Tag String deriving (Show, Y.ToJSON, Ord, Eq)

data CompiledModuleCall = CompiledModuleCall T.Text T.Text deriving Show

class ModuleCall m where
  compile :: m -> CompiledModuleCall

data Task = Task
  { taskName :: String
  , taskTags :: [Tag]
  , moduleCall :: CompiledModuleCall
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
    let (CompiledModuleCall moduleName args) = moduleCall t
    in  Y.object
          [ "name"     .= taskName t
          , "tags"     .= taskTags t
          , moduleName .= args
          ]

instance Y.ToJSON Playbook where
  toJSON p =
    let opts = catMaybes
                 [ ("sudo" .= )      <$> pbSudo p
                 , ("sudo_user" .= ) <$> pbSudoUser p
                 ]
    in  Y.object $
          [ "hosts" .= pbHosts p
          , "tags"  .= pbTags p
          , "tasks" .= pbTasks p
          ] ++ opts

encode :: [Playbook] -> BS.ByteString
encode pbs = "---\n" <> Y.encode pbs
