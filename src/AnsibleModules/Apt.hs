module AnsibleModules.Apt where

import Data.Sansible
import qualified Data.Aeson.TH as A

import qualified Data.Text as T

data AptState = Latest | Absent | Present
$(A.deriveToJSON encodingOptions ''AptState)

data AptUpgrade = Yes | Safe | Full | Dist
$(A.deriveToJSON encodingOptions ''AptUpgrade)

data Apt = Apt
         { name              :: Maybe T.Text
         , state             :: Maybe AptState
         , updateCache       :: Maybe Bool
         , cacheValidTime    :: Maybe Int
         , purge             :: Maybe Bool
         , defaultRelease    :: Maybe T.Text
         , installRecommends :: Maybe Bool
         , force             :: Maybe Bool
         , upgrade           :: Maybe AptUpgrade
         , dpkgOptions       :: Maybe T.Text
         , deb               :: Maybe FilePath
         }

defaultApt :: Apt
defaultApt = Apt
           { name              = Nothing
           , state             = Nothing
           , updateCache       = Nothing
           , cacheValidTime    = Nothing
           , purge             = Nothing
           , defaultRelease    = Nothing
           , installRecommends = Nothing
           , force             = Nothing
           , upgrade           = Nothing
           , dpkgOptions       = Nothing
           , deb               = Nothing
           }

instance ModuleCall Apt where
  moduleLabel _ = "apt"

$(A.deriveToJSON encodingOptions ''Apt)

oneDay :: Int
oneDay = 86400

-- | Install an apt package
aptInstall :: T.Text -> CompiledModuleCall
aptInstall pkg = compile $
  defaultApt { name           = Just pkg
             , cacheValidTime = Just oneDay
             , updateCache    = Just False
             }

-- | Update apt
aptUpdate :: CompiledModuleCall
aptUpdate = compile $
  defaultApt { cacheValidTime = Just oneDay
             , updateCache    = Just True
             }

-- | Install a Debian package from file
aptDebInstall :: FilePath -> CompiledModuleCall
aptDebInstall path = compile $
  defaultApt { deb = Just path }
