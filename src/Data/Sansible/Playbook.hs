module Data.Sansible.Playbook where
import Data.Maybe
import Data.Monoid
import Data.Set
import Data.Yaml ( (.=) )

import qualified Data.Aeson            as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.List             as L
import qualified Data.Text             as T
import qualified Data.Yaml             as Y

import Control.Applicative ((<$>))

import Data.Sansible

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
  { taskName        :: T.Text
  , moduleCall      :: CompiledModuleCall
  , taskTags        :: Set Tag
  , taskSudoUser    :: Maybe User
  } deriving (Show)

tag :: Tag -> Task -> Task
tag t tsk = tsk { taskTags = insert t (taskTags tsk)}

instance Y.ToJSON Task where
  toJSON t =
    let (CompiledModuleCall modName args ff) = moduleCall t
        userKeys (User u)                    = ["sudo_user" .= u]
        sudoKeys'                            = userKeys <$> taskSudoUser t
        sudoKeys                             = fromMaybe [] sudoKeys'
    in  Y.object $
          [ "name"  .= taskName t
          , "tags"  .= taskTags t
          , modName .= fromMaybe "" ff
          , "args"  .= args
          ] ++ sudoKeys

-- ^ shortcut to create a task without tags
task :: T.Text -> CompiledModuleCall -> Task
task n m = Task n m mempty Nothing

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

encode :: [Playbook] -> BS.ByteString
encode pbs = "---\n" <> Y.encode pbs
