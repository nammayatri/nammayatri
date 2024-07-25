module Lib.Yudhishthira.Types.Tag where

import Kernel.Prelude
import Kernel.Types.Common

data NammaTagResponse = NammaTagResponse
  { tagName :: Text,
    tagValue :: Text,
    tagCategory :: Text,
    tagValidity :: Maybe Hours
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)
