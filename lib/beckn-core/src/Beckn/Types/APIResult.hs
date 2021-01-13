module Beckn.Types.APIResult (APIResult (..)) where

import Data.Aeson (object, (.=))
import EulerHS.Prelude hiding ((.=))

data APIResult = Success | Failure Text

instance ToJSON APIResult where
  toJSON Success = object ["result" .= ("Success" :: Text)]
  toJSON (Failure message) = object ["result" .= ("Failure" :: Text), "message" .= message]
