{-# LANGUAGE DeriveAnyClass #-}

module Beckn.Types.Error where

import EulerHS.Prelude


data JsonError
  = JsonError
      { _errorCode :: Text,
        _errorMessage :: Text,
        _action :: Text
      }
  deriving (Generic)

instance FromJSON JsonError where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON JsonError where
  toJSON = genericToJSON stripLensPrefixOptions
