module Beckn.Types.Core.Migration.Offer (Offer (..)) where

import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Time (Time)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)

data Offer = Offer
  { id :: Maybe Text,
    descriptor :: Maybe Descriptor,
    location_ids :: Maybe [Text],
    category_ids :: Maybe [Text],
    item_ids :: Maybe [Text],
    time :: Maybe Time
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
