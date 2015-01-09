module AnsibleModules.Service where

import Data.Monoid
import Data.Sansible
import Data.Sansible.Playbook

import qualified Data.Aeson.TH as A
import qualified Data.Text     as T


data ServiceState = Started | Stopped | Restarted | Reloaded
$(A.deriveToJSON encodingOptions ''ServiceState)

data Service = Service
             { arguments :: Maybe T.Text
             , enabled   :: Maybe Bool
             , name      :: T.Text
             , pattern   :: Maybe T.Text
             , runLevel  :: Maybe T.Text
             , sleep     :: Maybe Bool
             , state     :: Maybe ServiceState
             }
$(A.deriveToJSON encodingOptions ''Service)

instance ModuleCall Service where
  moduleLabel _ = "service"

defaultService :: T.Text -> ServiceState -> Service
defaultService n s = Service
                   { arguments = Nothing
                   , enabled   = Nothing
                   , name      = n
                   , pattern   = Nothing
                   , runLevel  = Nothing
                   , sleep     = Nothing
                   , state     = Just s
                   }

simpleService :: T.Text -> ServiceState -> CompiledModuleCall
simpleService n s = compile $ defaultService n s


serviceTask :: T.Text -> ServiceState -> Task
serviceTask n s =
  task ("Starting service" <> n)
       (simpleService n s)
