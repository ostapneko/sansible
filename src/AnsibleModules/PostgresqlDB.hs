module AnsibleModules.PostgresqlDB where

import Data.Sansible
import Data.Sansible.Playbook

import qualified Data.Text as T
import qualified Data.Aeson.TH as A

data State = Present | Absent
$(A.deriveToJSON encodingOptions ''State)

data PostgresqlDB = PostgresqlDB
                  { name            :: T.Text
                  , loginUser       :: Maybe T.Text
                  , loginPassword   :: Maybe T.Text
                  , loginHost       :: Maybe T.Text
                  , loginUnixSocket :: Maybe FilePath
                  , owner           :: Maybe T.Text
                  , port            :: Maybe Int
                  , template        :: Maybe T.Text
                  , encoding        :: Maybe T.Text
                  , lcCollate       :: Maybe T.Text
                  , lcCtype         :: Maybe T.Text
                  , state           :: Maybe State
                  }
$(A.deriveToJSON encodingOptions ''PostgresqlDB)

instance ModuleCall PostgresqlDB where
  moduleLabel _ = "postgresql_db"

defaultPostgresqlDB :: T.Text -> PostgresqlDB
defaultPostgresqlDB n = PostgresqlDB
                  { name            = n
                  , loginUser       = Nothing
                  , loginPassword   = Nothing
                  , loginHost       = Nothing
                  , loginUnixSocket = Nothing
                  , owner           = Nothing
                  , port            = Nothing
                  , template        = Nothing
                  , encoding        = Nothing
                  , lcCollate       = Nothing
                  , lcCtype         = Nothing
                  , state           = Nothing
                  }

simplePostgresqlDB :: T.Text              -- ^ db name
                   -> T.Text              -- ^ db owner
                   -> CompiledModuleCall
simplePostgresqlDB n o = compile $ (defaultPostgresqlDB n) { owner = Just o }
