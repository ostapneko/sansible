module AnsibleModules.GetUrl where

import Data.Monoid
import Data.Sansible
import Data.Sansible.Playbook
import Network.URI

import qualified Data.Aeson.TH as A

import qualified Data.Text as T

data GetUrl = GetUrl
            { dest           :: FilePath
            , force          :: Maybe Bool
            , sha256sum      :: Maybe T.Text
            , timeout        :: Maybe Int
            , url            :: URI
            , url_password   :: Maybe T.Text
            , url_username   :: Maybe T.Text
            , use_proxy      :: Maybe Bool
            , validate_certs :: Maybe Bool
            , mode           :: Maybe T.Text
            }

$(A.deriveToJSON encodingOptions ''GetUrl)

instance ModuleCall GetUrl where
  moduleLabel _ = "get_url"

defaultGetUrl :: URI -> FilePath -> Maybe T.Text -> GetUrl
defaultGetUrl url' dest' m = GetUrl
                           { dest           = dest'
                           , force          = Just False
                           , sha256sum      = Nothing
                           , timeout        = Nothing
                           , url            = url'
                           , url_password   = Nothing
                           , url_username   = Nothing
                           , use_proxy      = Just True
                           , validate_certs = Just True
                           , mode           = m
                           }

download :: URI -> FilePath -> Maybe T.Text -> CompiledModuleCall
download url' dest' m = compile $ defaultGetUrl url' dest' m

downloadTask :: URI -> FilePath -> Maybe T.Text -> Task
downloadTask u d m =
  task ("downloading " <> T.pack (show u))
       (download u d m)