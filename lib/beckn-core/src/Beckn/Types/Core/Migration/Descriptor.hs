{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Descriptor (Descriptor (..)) where

import Beckn.Types.Core.Migration.Image (Image)
import Data.Aeson.TH (deriveJSON)
import EulerHS.Prelude
import Servant.Client (BaseUrl)

data Descriptor = Descriptor
  { _name :: Maybe Text,
    _code :: Maybe Text,
    _symbol :: Maybe Text,
    _short_desc :: Maybe Text,
    _long_desc :: Maybe Text,
    _images :: [Image],
    _audio :: Maybe BaseUrl,
    _3d_render :: Maybe BaseUrl
  }
  deriving (Generic, Show)

deriveJSON stripAllLensPrefixOptions ''Descriptor
