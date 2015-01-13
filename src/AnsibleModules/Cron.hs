module AnsibleModules.Cron where

import Data.Sansible
import Data.Sansible.Playbook

import qualified Data.Text as T
import qualified Data.Aeson.TH as A

data State = Present | Absent deriving (Show, Eq)
$(A.deriveToJSON encodingOptions ''State)

data SpecialTime
    = Reboot
    | Yearly
    | Annually
    | Monthly
    | Weekly
    | Daily
    | Hourly
    deriving (Show, Eq)
$(A.deriveToJSON encodingOptions ''SpecialTime)

data Cron = Cron
          { name       :: T.Text
          , backup     :: Maybe Bool
          , cronFile   :: Maybe FilePath
          , day        :: Maybe T.Text
          , hour       :: Maybe T.Text
          , job        :: Maybe T.Text
          , minute     :: Maybe T.Text
          , month      :: Maybe T.Text
          , speciaTime :: Maybe SpecialTime
          , state      :: Maybe State
          , user       :: Maybe User
          , weekDay    :: Maybe T.Text
          }

$(A.deriveToJSON encodingOptions ''Cron)
instance ModuleCall Cron where
  moduleLabel _ = "cron"

defaultCron :: T.Text -> Cron
defaultCron n =
  Cron
  { name       = n
  , backup     = Nothing
  , cronFile   = Nothing
  , day        = Nothing
  , hour       = Nothing
  , job        = Nothing
  , minute     = Nothing
  , month      = Nothing
  , speciaTime = Nothing
  , state      = Nothing
  , user       = Nothing
  , weekDay    = Nothing
  }

nightlyCron :: User
                -> T.Text
                -> T.Text
                -> CompiledModuleCall
nightlyCron u n j =
  compile $ (defaultCron n)
            { name = n
            , user = Just u
            , job  = Just j
            , hour = Just "0"
            }
