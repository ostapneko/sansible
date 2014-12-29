{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Sansible.Common where

import Data.String

import qualified Data.Aeson            as A
import qualified Data.Text             as T
import qualified Data.Yaml             as Y

import Network.URI

newtype HostPattern = HostPattern T.Text deriving (Show, Y.ToJSON, IsString)
newtype User        = User T.Text        deriving (Show, Y.ToJSON, IsString)
newtype Group       = Group T.Text       deriving (Show, Y.ToJSON, IsString)
newtype Tag         = Tag T.Text         deriving (Show, Y.ToJSON, IsString, Ord, Eq)

instance A.ToJSON URI where
  toJSON = A.toJSON . show
