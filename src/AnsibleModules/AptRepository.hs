module AnsibleModules.AptRepository where

import Data.Monoid
import Data.Sansible
import Data.Sansible.Playbook
import qualified Data.Aeson.TH as A
import qualified Data.Text as T

data State = Present | Absent
$(A.deriveToJSON encodingOptions ''State)

data AptRepository = AptRepository
            { repo          :: T.Text
            , state         :: Maybe State
            , mode          :: Maybe T.Text
            , updateCache   :: Maybe Bool
            , validateCerts :: Maybe Bool
            }
$(A.deriveToJSON encodingOptions ''AptRepository)

instance ModuleCall AptRepository where
  moduleLabel _ = "apt_repository"

defaultAptRepository :: T.Text -> AptRepository
defaultAptRepository r = AptRepository
                       { repo          = r
                       , state         = Nothing
                       , mode          = Nothing
                       , updateCache   = Nothing
                       , validateCerts = Nothing
                       }

addAptRepo :: T.Text -> CompiledModuleCall
addAptRepo r = compile $ (defaultAptRepository r) { state = Just Present }

addAptRepoTask :: T.Text -> Task
addAptRepoTask r =
  task ("Adding apt-repo " <> r)
       (addAptRepo r)
