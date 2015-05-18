module AnsibleModules.Hostname
 ( Hostname(..)
 , simpleHostname
 , hostnameTask
 ) where

import qualified Data.Text as T
import Data.Monoid
import Data.Sansible
import Data.Sansible.Playbook

import qualified Data.Aeson.TH          as A

data Hostname = Hostname { name :: T.Text }
$(A.deriveToJSON S.encodingOptions ''Hostname)

instance ModuleCall Hostname where
  moduleLabel _ = "hostname"

defaultHostname :: T.Text = Hostname

hostnameTask :: T.Text -> Task
hostnameTask n =
  task ("Setting hostname to " <> n) (compile $ defaultHostname n)
