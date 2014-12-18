module AnsibleModules.Apt where

import Data.Sansible
import Data.Monoid

import qualified Data.Text as T

data AptModuleCall = AptModuleCall T.Text
instance ModuleCall AptModuleCall where
  compile (AptModuleCall pkg) =
    let args = "name=" <> pkg
    in  CompiledModuleCall "apt" args

