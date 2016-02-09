module AnsibleModules.DebConf
 ( DebConf(..)
 , ValueType(..)
 , defaultDebConf
 , debConfTask
 ) where

import Data.Monoid
import Data.Sansible.Playbook

import qualified Data.Text     as T
import qualified Data.Aeson    as A
import Data.Aeson ((.=))

data ValueType = VSelect T.Text deriving Show

valueType :: ValueType ->  (T.Text, T.Text)
valueType vt = case vt of
  VSelect v -> (v, "select")

data DebConf = DebConf
             { name :: T.Text
             , question :: T.Text
             , value :: ValueType
             }

instance ModuleCall DebConf where
  moduleLabel _ = "debconf"

instance A.ToJSON DebConf where
  toJSON c = A.object attrs
    where
      vt = valueType $ value c
      attrs = [ "name"     .= name c
              , "question" .= question c
              , "value"    .= fst vt
              , "vtype"    .= snd vt
              ]

defaultDebConf :: T.Text -> T.Text -> ValueType -> DebConf
defaultDebConf n q v = DebConf { name = n
                               , question = q
                               , value = v
                               }

debConfTask :: T.Text -> T.Text -> ValueType -> Task
debConfTask n q v =
  let msg = "Set debconf: " <> n <> " "  <> q <> " to " <> T.pack (show v)
  in task msg $ compile (defaultDebConf n q v)
