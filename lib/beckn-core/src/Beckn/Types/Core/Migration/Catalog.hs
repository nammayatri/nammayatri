module Beckn.Types.Core.Migration.Catalog (Catalog (..)) where

import Beckn.Types.Core.Migration.Category (Category)
import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Fulfillment (Fulfillment)
import Beckn.Types.Core.Migration.Offer (Offer)
import Beckn.Types.Core.Migration.Payment (Payment)
import Beckn.Types.Core.Migration.Provider (Provider)
import Beckn.Utils.JSON (slashedRecordFields)
import Data.Time (UTCTime)
import EulerHS.Prelude

data Catalog = Catalog
  { _bpp_descriptor :: Maybe Descriptor,
    _bpp_categories :: Maybe [Category],
    _bpp_fulfillments :: Maybe [Fulfillment],
    _bpp_payments :: Maybe [Payment],
    _bpp_offers :: Maybe [Offer],
    _bpp_providers :: Maybe [Provider],
    _exp :: Maybe UTCTime
  }
  deriving (Generic)

instance FromJSON Catalog where
  parseJSON = genericParseJSON slashedRecordFields

instance ToJSON Catalog where
  toJSON = genericToJSON slashedRecordFields
