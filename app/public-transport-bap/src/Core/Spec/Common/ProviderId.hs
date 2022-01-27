module Core.Spec.Common.ProviderId where

import Beckn.Prelude
import Beckn.Utils.GenericPretty

newtype ProviderId = ProviderId
  { id :: Text
  }
  deriving (Generic, FromJSON, Show, ToJSON, ToSchema, PrettyShow)
