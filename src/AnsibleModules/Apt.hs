{-# LANGUAGE TupleSections #-}

module AnsibleModules.Apt where

import Control.Applicative
import Data.Maybe
import Data.Sansible

import qualified Data.Text as T

data Apt = Apt
         { name           :: Maybe T.Text
         , cacheValidTime :: Maybe Int
         , updateCache    :: Maybe Bool
         }

defaultApt :: T.Text -> Apt
defaultApt pkg = Apt
               { name = Just pkg
               , cacheValidTime = Just 86400 -- 1 day
               , updateCache = Just False
               }

defaultUpdateApt :: Apt
defaultUpdateApt = Apt
                 { name = Nothing
                 , cacheValidTime = Just 86400
                 , updateCache = Just True
                 }

instance ModuleCall Apt where
  compile apt =
    let opts = catMaybes [ ("name",) . AnsibleValue <$> name apt
                         , ("cache_valid_time",) . AnsibleValue . T.pack . show <$> cacheValidTime apt
                         , ("update_cache",) . AnsibleValue . T.pack . show <$> updateCache apt
                         ]
    in  CompiledModuleCall "apt" $ opts
