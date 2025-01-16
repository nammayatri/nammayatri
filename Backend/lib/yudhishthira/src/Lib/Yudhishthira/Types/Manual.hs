module Lib.Yudhishthira.Types.Manual where

import Kernel.Prelude
import Lib.Yudhishthira.Types.Common

data NammaTagManual = NammaTagManual
  { tagCategory :: Text,
    description :: Maybe Text,
    tagName :: Text,
    tagPossibleValues :: TagValues
  }
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)
