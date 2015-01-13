module AnsibleModules.Lineinfile where

import Data.Sansible
import Data.Sansible.Playbook

import qualified Data.Aeson.TH as A
import qualified Data.Text as T

data State = Present | Absent
$(A.deriveToJSON encodingOptions ''State)

data Lineinfile = Lineinfile
                { dest         :: FilePath
                , regexp       :: Maybe T.Text
                , state        :: Maybe State
                , line         :: Maybe T.Text
                , backrefs     :: Maybe Bool
                , insertafter  :: Maybe T.Text
                , insertbefore :: Maybe T.Text
                , create       :: Maybe Bool
                , backup       :: Maybe Bool
                , validate     :: Maybe Bool
                , follow       :: Maybe Bool
                , force        :: Maybe Bool
                , group        :: Maybe Group
                , mode         :: Maybe T.Text
                , owner        :: Maybe User
                , recurse      :: Maybe Bool
                , selevel      :: Maybe T.Text
                , serole       :: Maybe T.Text
                , setype       :: Maybe T.Text
                , seuser       :: Maybe T.Text
                }

defaultLineinfile :: FilePath -> Lineinfile
defaultLineinfile p = Lineinfile
                    { dest         = p
                    , regexp       = Nothing
                    , state        = Nothing
                    , line         = Nothing
                    , backrefs     = Nothing
                    , insertafter  = Nothing
                    , insertbefore = Nothing
                    , create       = Nothing
                    , backup       = Nothing
                    , validate     = Nothing
                    , follow       = Nothing
                    , force        = Nothing
                    , group        = Nothing
                    , mode         = Nothing
                    , owner        = Nothing
                    , recurse      = Nothing
                    , selevel      = Nothing
                    , serole       = Nothing
                    , setype       = Nothing
                    , seuser       = Nothing
                    }

instance ModuleCall Lineinfile where
  moduleLabel _ = "lineinfile"

$(A.deriveToJSON encodingOptions ''Lineinfile)

simpleLineinfile :: FilePath -> T.Text -> Lineinfile
simpleLineinfile p l = (defaultLineinfile p) { line = Just l }
