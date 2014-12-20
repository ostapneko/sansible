module AnsibleModules.Apt where

import Data.Sansible
import qualified Data.Aeson.TH as A

import qualified Data.Text as T

data Apt = Apt
         { name           :: Maybe T.Text
         , cacheValidTime :: Maybe Int
         , updateCache    :: Maybe Bool
         }

instance ModuleCall Apt where
  moduleLabel _ = "apt"

$(A.deriveToJSON encodingOptions ''Apt)

aptInstall :: T.Text -> CompiledModuleCall
aptInstall pkg = compile $ Apt
               { name = Just pkg
               , cacheValidTime = Just 86400 -- 1 day
               , updateCache = Just False
               }

aptUpdate :: CompiledModuleCall
aptUpdate = compile $ Apt
          { name = Nothing
          , cacheValidTime = Just 86400
          , updateCache = Just True
          }
