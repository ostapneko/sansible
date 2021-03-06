module AnsibleModules.Apt where

import Data.Monoid
import Data.Sansible
import Data.Sansible.Playbook

import qualified Data.Aeson.TH as A
import qualified Data.Text as T

data State = Latest | Absent | Present
$(A.deriveToJSON encodingOptions ''State)

data Upgrade = Yes | Safe | Full | Dist
$(A.deriveToJSON encodingOptions ''Upgrade)

data Apt = Apt
         { name              :: Maybe T.Text
         , state             :: Maybe State
         , updateCache       :: Maybe Bool
         , cacheValidTime    :: Maybe Int
         , purge             :: Maybe Bool
         , defaultRelease    :: Maybe T.Text
         , installRecommends :: Maybe Bool
         , force             :: Maybe Bool
         , upgrade           :: Maybe Upgrade
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

aptInstallTask :: T.Text -> Task
aptInstallTask p =
  task ("Install apt package " <> p) (aptInstall p)

-- | Update apt
aptUpdate :: CompiledModuleCall
aptUpdate = compile $
  defaultApt { cacheValidTime = Just oneDay
             , updateCache    = Just True
             }

aptUpdateTask :: Task
aptUpdateTask =
  task "Run apt-get update" aptUpdate

forceAptUpdateTask :: Task
forceAptUpdateTask =
  task "Run apt-get update" $ compile $ defaultApt { cacheValidTime = Nothing
                                                   , updateCache    = Just True
                                                   }

-- | Install a Debian package from file
aptDebInstall :: FilePath -> CompiledModuleCall
aptDebInstall path = compile $
  defaultApt { deb = Just path }

aptDebInstallTask :: FilePath -> Task
aptDebInstallTask p =
  task ("Install deb package " <> T.pack p) (aptDebInstall p)
