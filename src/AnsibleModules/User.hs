{-# LANGUAGE FlexibleInstances #-}
module AnsibleModules.User
  ( User(..)
  , UserState(..)
  , UserUpdatePassword(..)
  , defaultUser
  , simpleCreateUser
  ) where

import Data.Sansible hiding (User)
import Data.Sansible.Playbook
import qualified Data.Sansible as S

import Data.Aeson ((.=))
import qualified Data.Aeson    as A
import qualified Data.Aeson.TH as A
import qualified Data.Text     as T

data UserState = Present | Absent
$(A.deriveToJSON encodingOptions ''UserState)

data UserUpdatePassword = Always | OnCreate
$(A.deriveToJSON encodingOptions ''UserUpdatePassword)

data User = User
           { name             :: S.User
           , comment          :: Maybe T.Text
           , uid              :: Maybe Int
           , nonUnique        :: Maybe Bool
           , group            :: Maybe S.Group
           , groups           :: Maybe [S.Group]
           , append           :: Maybe Bool
           , shell            :: Maybe FilePath
           , home             :: Maybe FilePath
           , password         :: Maybe T.Text
           , state            :: Maybe UserState
           , createhome       :: Maybe Bool
           , moveHome         :: Maybe Bool
           , system           :: Maybe Bool
           , force            :: Maybe Bool
           , loginClass       :: Maybe T.Text
           , remove           :: Maybe Bool
           , generateSshKey   :: Maybe Bool
           , sshKeyBits       :: Maybe Int
           , sshKeyType       :: Maybe T.Text
           , sshKeyFile       :: Maybe FilePath
           , sshKeyComment    :: Maybe T.Text
           , sshKeyPassphrase :: Maybe T.Text
           , updatePassword   :: Maybe UserUpdatePassword
           }

defaultUser :: S.User -> User
defaultUser user = User
                 { name             = user
                 , comment          = Nothing
                 , uid              = Nothing
                 , nonUnique        = Nothing
                 , group            = Nothing
                 , groups           = Nothing
                 , append           = Nothing
                 , shell            = Nothing
                 , home             = Nothing
                 , password         = Nothing
                 , state            = Nothing
                 , createhome       = Nothing
                 , moveHome         = Nothing
                 , system           = Nothing
                 , force            = Nothing
                 , loginClass       = Nothing
                 , remove           = Nothing
                 , generateSshKey   = Nothing
                 , sshKeyBits       = Nothing
                 , sshKeyType       = Nothing
                 , sshKeyFile       = Nothing
                 , sshKeyComment    = Nothing
                 , sshKeyPassphrase = Nothing
                 , updatePassword   = Nothing
                 }

instance ModuleCall User where
  moduleLabel _ = "user"

instance A.ToJSON User where
  toJSON u = args
    where
      gs    = maybe "" (T.intercalate "," . map fromGroup) (groups u)
      args  = A.object $ filter ((/= A.Null) . snd)
                                [ "name"               .= name u
                                , "comment"            .= comment u
                                , "uid"                .= uid u
                                , "non_unique"         .= nonUnique u
                                , "group"              .= group u
                                , "groups"             .= gs
                                , "append"             .= append u
                                , "shell"              .= shell u
                                , "home"               .= home u
                                , "password"           .= password u
                                , "state"              .= state u
                                , "createhome"         .= createhome u
                                , "move_home"          .= moveHome u
                                , "system"             .= system u
                                , "force"              .= force u
                                , "login_class"        .= loginClass u
                                , "remove"             .= remove u
                                , "generate_ssh_key"   .= generateSshKey u
                                , "ssh_key_bits"       .= sshKeyBits u
                                , "ssh_key_type"       .= sshKeyType u
                                , "ssh_key_file"       .= sshKeyFile u
                                , "ssh_key_comment"    .= sshKeyComment u
                                , "ssh_key_passphrase" .= sshKeyPassphrase u
                                , "update_password"    .= updatePassword u
                                ]

simpleCreateUser :: S.User -> S.Group -> CompiledModuleCall
simpleCreateUser u g = compile $ (defaultUser u) { group = Just g }
