module AnsibleModules.PostgresqlUser where

import Data.List (intercalate)
import Data.Monoid
import Data.Sansible
import Data.Sansible.Playbook

import Data.Aeson ((.=))
import qualified Data.Aeson.TH as A
import qualified Data.Aeson    as A
import qualified Data.Text as T

data State = Present | Absent
$(A.deriveToJSON encodingOptions ''State)

data RoleAttrFlag = SuperUser Bool
                  | CreateRole Bool
                  | CreateUser Bool
                  | CreateDb Bool
                  | Inherit Bool
                  | Login Bool
                  | Replication Bool

encodeFlag :: RoleAttrFlag -> String
encodeFlag = show'
  where
    show' (SuperUser b)   = prefix b <> "SUPERUSER"
    show' (CreateRole b)  = prefix b <> "CREATEROLE"
    show' (CreateUser b)  = prefix b <> "CREATEUSER"
    show' (CreateDb b)    = prefix b <> "CREATEDB"
    show' (Inherit b)     = prefix b <> "INHERIT"
    show' (Login b)       = prefix b <> "LOGIN"
    show' (Replication b) = prefix b <> "REPLICATION"
    prefix b             = if b then "" else "NO"

data PostgresqlUser = PostgresqlUser
                  { db              :: Maybe T.Text
                  , encrypted       :: Maybe Bool
                  , expires         :: Maybe Bool
                  , failOnUser      :: Maybe Bool
                  , loginHost       :: Maybe T.Text
                  , loginPassword   :: Maybe T.Text
                  , loginUnixSocket :: Maybe FilePath
                  , loginUser       :: Maybe T.Text
                  , name            :: T.Text
                  , password        :: Maybe T.Text
                  , port            :: Maybe Int
                  , priv            :: Maybe T.Text
                  , roleAttrFlags   :: Maybe [RoleAttrFlag]
                  , state           :: Maybe State
                  }

instance ModuleCall PostgresqlUser where
  moduleLabel _ = "postgresql_user"

instance A.ToJSON PostgresqlUser where
  toJSON u = args
    where
      filterNull = filter ((/= A.Null) . snd)
      flags      = fmap (intercalate "," . map encodeFlag) (roleAttrFlags u)
      args       = A.object $ filterNull $ [ "db"              .= db u
                                           , "encrypted"       .= encrypted u
                                           , "expires"         .= expires u
                                           , "failOnUser"      .= failOnUser u
                                           , "loginHost"       .= loginHost u
                                           , "loginPassword"   .= loginPassword u
                                           , "loginUnixSocket" .= loginUnixSocket u
                                           , "loginUser"       .= loginUser u
                                           , "name"            .= name u
                                           , "password"        .= password u
                                           , "port"            .= port u
                                           , "priv"            .= priv u
                                           , "roleAttrFlags"   .= flags
                                           , "state"           .= state u
                                           ]

defaultPostgresqlUser :: T.Text -> PostgresqlUser
defaultPostgresqlUser n = PostgresqlUser
                          { db              = Nothing
                          , encrypted       = Nothing
                          , expires         = Nothing
                          , failOnUser      = Nothing
                          , loginHost       = Nothing
                          , loginPassword   = Nothing
                          , loginUnixSocket = Nothing
                          , loginUser       = Nothing
                          , name            = n
                          , password        = Nothing
                          , port            = Nothing
                          , priv            = Nothing
                          , roleAttrFlags   = Nothing
                          , state           = Nothing
                          }

postgresqlUser :: T.Text -> CompiledModuleCall
postgresqlUser = compile . defaultPostgresqlUser

postgresqlUserTask :: T.Text -> Task
postgresqlUserTask u =
  let label = "Creating postgres user " <> u
  in  task label (postgresqlUser u)

