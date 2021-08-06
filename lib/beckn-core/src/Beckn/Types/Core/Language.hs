module Beckn.Types.Core.Language where

import Data.Text
import EulerHS.Prelude

newtype Language = Language
  { code :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)
