{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Sansible
  ( HostPattern(..)
  , User(..)
  , Group(..)
  , Tag(..)
  , encodingOptions
  ) where

import Data.Maybe
import Data.Char

import qualified Data.Aeson            as A
import qualified Data.Aeson.TH         as A
import qualified Data.List             as L
import qualified Data.Text             as T
import qualified Data.Yaml             as Y

import Data.String

import Network.URI

newtype HostPattern = HostPattern T.Text deriving (Show, Y.ToJSON, IsString)
newtype User  = User  { fromUser :: T.Text } deriving (Show, Y.ToJSON, IsString)
newtype Group = Group { fromGroup :: T.Text } deriving (Show, Y.ToJSON, IsString)
newtype Tag   = Tag T.Text  deriving (Show, Y.ToJSON, IsString, Ord, Eq)

instance A.ToJSON URI where
  toJSON = A.toJSON . show

snakeCase :: String -> String
snakeCase str =
  let go [] = []
      go (c : cs) =
        if isUpper c
          then '_' : toLower c : go cs
          else c : go cs
      str' = go str
  in fromMaybe str' (L.stripPrefix "_" str')

encodingOptions :: A.Options
encodingOptions = A.defaultOptions
                { A.fieldLabelModifier = snakeCase
                , A.omitNothingFields = True
                , A.constructorTagModifier = stripChoice
                }

stripChoice :: String -> String
stripChoice str =
  let lower = snakeCase str
  in fromMaybe lower (L.stripPrefix "choice_" lower)
