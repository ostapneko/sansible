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
newtype User        = User T.Text        deriving (Show, Y.ToJSON, IsString)
newtype Group       = Group T.Text       deriving (Show, Y.ToJSON, IsString)
newtype Tag         = Tag T.Text         deriving (Show, Y.ToJSON, IsString, Ord, Eq)

instance A.ToJSON URI where
  toJSON = A.toJSON . show

snakeCase :: String -> String
snakeCase [] = []
snakeCase (c : cs) = if isUpper c
                     then '_' : toLower c : snakeCase cs
                     else c : snakeCase cs

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
