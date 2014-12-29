{-# LANGUAGE TupleSections #-}

module Data.Sansible.Inventory where

import Control.Applicative
import Data.Maybe
import Data.Monoid
import Data.Text.Encoding
import qualified Data.Text             as T
import qualified Data.ByteString.Char8 as BS

import Data.Sansible

data InventoryGroup = InventoryGroup
                    { inventoryGroupName :: Maybe T.Text
                    , systems            :: [System]
                    }

data System = System
            { systemName               :: T.Text
            , ansibleSshHost           :: Maybe HostPattern
            , ansibleSshPort           :: Maybe Int
            , ansibleSshUser           :: Maybe User
            , ansibleSshPass           :: Maybe T.Text
            , ansibleSudoPass          :: Maybe T.Text
            , ansibleSudoExe           :: Maybe FilePath
            , ansibleConnection        :: Maybe AnsibleConnection
            , ansibleSshPrivateKeyFile :: Maybe FilePath
            , ansibleShellType         :: Maybe T.Text
            , ansiblePythonInterpreter :: Maybe FilePath
            }

encodeSystem :: System -> BS.ByteString
encodeSystem s =
  let host = case ansibleSshHost s of
               Just (HostPattern h) -> Just $ encodeUtf8 h
               Nothing              -> Nothing
      user = case ansibleSshUser s of
               Just (User u) -> Just $ encodeUtf8 u
               Nothing       -> Nothing
      pairs = catMaybes
              [ ("ansible_ssh_host",)                       <$> host
              , ("ansible_ssh_port",) . BS.pack . show      <$> ansibleSshPort s
              , ("ansible_ssh_user",)                       <$> user
              , ("ansible_ssh_pass",) . encodeUtf8          <$> ansibleSshPass s
              , ("ansible_sudo_pass",) . encodeUtf8         <$> ansibleSudoPass s
              , ("ansible_sudo_exe",) . BS.pack . show      <$> ansibleSudoExe s
              , ("ansible_connection",) . BS.pack . show    <$> ansibleConnection s
              , ("ansible_ssh_private_key_file",) . BS.pack <$> ansibleSshPrivateKeyFile s
              , ("ansible_shell_type",) . encodeUtf8        <$> ansibleShellType s
              , ("ansible_python_interpreter",) . BS.pack   <$> ansiblePythonInterpreter s
              ]
      join (k, v) = k <> "=" <> v
  in BS.unwords $ encodeUtf8 (systemName s) : map join pairs

simpleSystem :: T.Text -> System
simpleSystem name = System
                    name
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing

data AnsibleConnection = AnsibleConnectionLocal
                       | AnsibleConnectionSSH
                       | AnsibleConnectionParamiko

instance Show AnsibleConnection where
  show AnsibleConnectionLocal    = "local"
  show AnsibleConnectionSSH      = "ssh"
  show AnsibleConnectionParamiko = "paramiko"

encode :: [InventoryGroup] -> BS.ByteString
encode igs =
  let encodeGroup (InventoryGroup name sys) =
        let encSys = map encodeSystem sys
            ls     = case name of
                       Just n  -> "[" <> encodeUtf8 n <> "]" : encSys
                       Nothing -> encSys
        in  BS.unlines ls
  in BS.intercalate "\n" $ map encodeGroup igs
