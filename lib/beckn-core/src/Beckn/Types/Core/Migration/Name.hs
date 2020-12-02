{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Name (Name (..)) where

import Data.Aeson.TH (deriveJSON)
import EulerHS.Prelude

data Name = Name
  { _full :: Maybe Text,
    _additional_name :: Maybe Text,
    _family_name :: Maybe Text,
    _given_name :: Maybe Text,
    _call_sign :: Maybe Text,
    _honorific_prefix :: Maybe Text,
    _honorific_suffix :: Maybe Text
  }
  deriving (Generic, Show)

deriveJSON stripLensPrefixOptions ''Name

{- DELETEME: Should it be something more like this?
data Name
  = FullName Text
  | Deconstructed
    { _additional_name :: Maybe Text,
      _family_name :: Maybe Text,
      _given_name :: Maybe Text,
      _call_sign :: Maybe Text,
      _honorific_prefix :: Maybe Text,
      _honorific_suffix :: Maybe Text
    }
-}
