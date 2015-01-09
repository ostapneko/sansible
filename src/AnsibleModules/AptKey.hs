module AnsibleModules.AptKey where

import Network.URI

import Data.Monoid
import Data.Maybe
import Data.Sansible
import Data.Sansible.Playbook
import qualified Data.Aeson.TH as A
import qualified Data.Text as T

data AptKeyState = Present | Absent
$(A.deriveToJSON encodingOptions ''AptKeyState)

data AptKey = AptKey
            { id_           :: Maybe T.Text
            , data_         :: Maybe T.Text
            , file          :: Maybe FilePath
            , keyring       :: Maybe FilePath
            , url           :: Maybe URI
            , keyserver     :: Maybe T.Text
            , state         :: Maybe AptKeyState
            , validateCerts :: Maybe Bool
            }
$(A.deriveToJSON encodingOptions ''AptKey)

instance ModuleCall AptKey where
  moduleLabel _ = "apt_key"

defaultAptKey :: AptKey
defaultAptKey = AptKey
              { id_           = Nothing
              , data_         = Nothing
              , file          = Nothing
              , keyring       = Nothing
              , url           = Nothing
              , keyserver     = Nothing
              , state         = Nothing
              , validateCerts = Nothing
              }

unsafeFromUrl :: String -> CompiledModuleCall
unsafeFromUrl urlStr =
  let u = fromJust $ parseURI urlStr
  in compile $ defaultAptKey { url   = Just u
                             , state = Just Present
                             }

unsafeFromUrlTask :: String -> Task
unsafeFromUrlTask u =
  task ("Add apt-key: " <> T.pack u) (unsafeFromUrl u)
