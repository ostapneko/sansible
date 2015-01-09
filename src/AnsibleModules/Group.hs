module AnsibleModules.Group where

import Data.Monoid
import Data.Sansible hiding (Group)
import Data.Sansible.Playbook
import qualified Data.Sansible as S

import qualified Data.Aeson.TH as A

data GroupState = Present | Absent
$(A.deriveToJSON encodingOptions ''GroupState)

data Group = Group
           { name   :: S.Group
           , gid    :: Maybe Int
           , state  :: Maybe GroupState
           , system :: Maybe Bool
           }

instance ModuleCall Group where
  moduleLabel _ = "group"
$(A.deriveToJSON encodingOptions ''Group)

defaultGroup :: S.Group -> Group
defaultGroup g = Group
               { name   = g
               , gid    = Nothing
               , state  = Nothing
               , system = Nothing
               }

createGroup :: S.Group -> CompiledModuleCall
createGroup = compile . defaultGroup

createGroupTask :: S.Group -> Task
createGroupTask g =
  task ("Creating group " <> fromGroup g)
       (createGroup g)
