module Beckn.Types.Core.Migration.Catalog (Catalog (..)) where

import Beckn.Types.Core.Migration.Category (Category)
import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Fulfillment (Fulfillment)
import Beckn.Types.Core.Migration.Offer (Offer)
import Beckn.Types.Core.Migration.Payment (Payment)
import Beckn.Types.Core.Migration.Provider (Provider)
import Beckn.Utils.Example
import Beckn.Utils.JSON (slashedRecordFields)
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (exp, id)

data Catalog = Catalog
  { bpp_descriptor :: Maybe Descriptor,
    bpp_categories :: Maybe [Category],
    bpp_fulfillments :: Maybe [Fulfillment],
    bpp_payments :: Maybe [Payment],
    bpp_offers :: Maybe [Offer],
    bpp_providers :: Maybe [Provider],
    exp :: Maybe UTCTime
  }
  deriving (Generic, Show)

instance FromJSON Catalog where
  parseJSON = genericParseJSON slashedRecordFields

instance ToJSON Catalog where
  toJSON = genericToJSON slashedRecordFields

instance Example Catalog where
  example =
    Catalog
      { bpp_descriptor = Nothing,
        bpp_categories = Nothing,
        bpp_fulfillments = Nothing,
        bpp_payments = Nothing,
        bpp_offers = Nothing,
        bpp_providers = Just $ one example,
        exp = Nothing
      }
