module AnsibleModules.GetUrl where

import Data.Sansible
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
            }

$(A.deriveToJSON encodingOptions ''GetUrl)

instance ModuleCall GetUrl where
  moduleLabel _ = "get_url"

defaultGetUrl :: URI -> FilePath -> GetUrl
defaultGetUrl url' dest' = GetUrl
                         { dest           = dest'
                         , force          = Just False
                         , sha256sum      = Nothing
                         , timeout        = Nothing
                         , url            = url'
                         , url_password   = Nothing
                         , url_username   = Nothing
                         , use_proxy      = Just True
                         , validate_certs = Just True
                         }

download :: URI -> FilePath -> CompiledModuleCall
download url' dest' = compile $ defaultGetUrl url' dest'
