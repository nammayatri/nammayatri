module Lib.Yudhishthira.Types.Tag where

import Kernel.Prelude
import Kernel.Types.Common

data TagValue
  = TextValue Text
  | NumberValue Double -- Int
  | ArrayValue [Text]
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data NammaTagResponse = NammaTagResponse
  { tagName :: Text,
    tagValue :: TagValue,
    tagCategory :: Text,
    tagValidity :: Maybe Hours
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)
