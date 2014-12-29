{-# LANGUAGE TupleSections #-}

module Data.Sansible.Inventory where

import Control.Applicative
import Data.Maybe
import qualified Data.Text             as T

import Data.Sansible.Common

data Inventory = Inventory { inventoryGroups :: [InventoryGroup] }

data InventoryGroup = InventoryGroup
                    { inventoryGroupName :: Maybe T.Text
                    , systems            :: [System]
                    }

data System = System
            { ansibleSshHost           :: HostPattern
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

instance Show System where
  show s =
    let (HostPattern host) = ansibleSshHost s
        user = case ansibleSshUser s of
                 Just (User u) -> Just u
                 Nothing       -> Nothing
        pairs = catMaybes
                [ Just ("ansible_ssh_host", T.unpack host)
                , ("ansible_ssh_port",) . show       <$> ansibleSshPort s
                , ("ansible_ssh_user",) . T.unpack   <$> user
                , ("ansible_ssh_pass",) . T.unpack   <$> ansibleSshPass s
                , ("ansible_sudo_pass",) . T.unpack  <$> ansibleSudoPass s
                , ("ansible_sudo_exe",)              <$> ansibleSudoExe s
                , ("ansible_connection",) . show     <$> ansibleConnection s
                , ("ansible_ssh_private_key_file",)  <$> ansibleSshPrivateKeyFile s
                , ("ansible_shell_type",) . T.unpack <$> ansibleShellType s
                , ("ansible_python_interpreter",)    <$> ansiblePythonInterpreter s
                ]
        join (k, v) = k ++ '=' : v
    in unwords $ map join pairs

data AnsibleConnection = AnsibleConnectionLocal
                       | AnsibleConnectionSSH
                       | AnsibleConnectionParamiko

instance Show AnsibleConnection where
  show AnsibleConnectionLocal    = "local"
  show AnsibleConnectionSSH      = "ssh"
  show AnsibleConnectionParamiko = "paramiko"
