module Data.Sansible where

import Control.Applicative
import Data.String
import Data.Maybe
import Data.Monoid
import Data.Set
import Data.Yaml ( (.=) )

import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Yaml as Y

newtype HostPattern = HostPattern String deriving (Show, Y.ToJSON)
newtype User = User String deriving (Show, Y.ToJSON)
newtype Tag  = Tag String deriving (Show, Y.ToJSON, Ord, Eq)

newtype AnsibleKey   = AnsibleKey T.Text   deriving (Show, Y.ToJSON, IsString)
newtype AnsibleValue = AnsibleValue T.Text deriving (Show, Y.ToJSON, IsString)

data CompiledModuleCall = CompiledModuleCall
                        { moduleName :: T.Text
                        , moduleCallArgs :: [(AnsibleKey, AnsibleValue)]
                        } deriving Show

class ModuleCall m where
  compile :: m -> CompiledModuleCall

data Task = Task
  { taskName :: T.Text
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
    let (CompiledModuleCall modName args) = moduleCall t
        argsStr = T.unwords $ L.map (\ (AnsibleKey k, AnsibleValue v) -> k <> "=" <> v) args
    in  Y.object
          [ "name"  .= taskName t
          , "tags"  .= taskTags t
          , modName .= argsStr
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
