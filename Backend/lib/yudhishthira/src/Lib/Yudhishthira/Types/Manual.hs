module Lib.Yudhishthira.Types.Manual where

import Kernel.Prelude
import Kernel.Types.Common
import Lib.Yudhishthira.Types.Common

data NammaTagManual = NammaTagManual
  { tagCategory :: Text,
    description :: Maybe Text,
    tagName :: Text,
    tagPossibleValues :: TagValues,
    tagValidity :: Maybe Hours,
    allowMultipleValues :: Maybe Bool
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)
