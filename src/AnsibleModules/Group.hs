module AnsibleModules.Group where

import Data.Sansible hiding (Group)
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
