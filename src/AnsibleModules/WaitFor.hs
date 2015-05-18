module AnsibleModules.WaitFor
 ( WaitFor(..)
 , defaultWaitFor
 , waitForTask
 ) where

import qualified Data.Sansible as S

import Data.Monoid
import Data.Sansible.Playbook

import qualified Data.Text     as T
import qualified Data.Aeson.TH as A

data State = Present
           | Started
           | Stopped
           | Drained
           | Absent
$(A.deriveToJSON S.encodingOptions ''State)

data WaitFor = WaitFor
             { connectionTimeout :: Maybe Int
             , delay             :: Maybe Int
             , excludeHosts      :: Maybe [S.HostPattern]
             , path              :: Maybe FilePath
             , port              :: Maybe Int
             , searchRegex       :: Maybe T.Text
             , timeout           :: Maybe Int
             , state             :: State
             }

$(A.deriveToJSON S.encodingOptions ''WaitFor)

instance ModuleCall WaitFor where
  moduleLabel _ = "wait_for"

defaultWaitFor :: Int -> Int -> WaitFor
defaultWaitFor p t =
  WaitFor { connectionTimeout = Nothing
          , delay             = Just 1
          , excludeHosts      = Nothing
          , path              = Nothing
          , port              = Just p
          , searchRegex       = Nothing
          , timeout           = Just t
          , state             = Started
          }

waitForTask :: Int -> Int -> Task
waitForTask p t =
  task ("Waiting for service to start on port " <> T.pack (show p))
       (compile $ defaultWaitFor p t)
