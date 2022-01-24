-- module Beckn.Types.Core.Migration.Catalog (Catalog (..)) where

module Core.Catalog where

-- import Beckn.Types.Core.Migration.Category (Category)

-- import Beckn.Types.Core.Migration.Fulfillment (Fulfillment)
-- import Beckn.Types.Core.Migration.Offer (Offer)
-- import Beckn.Types.Core.Migration.Payment (Payment)

-- import Data.Time (UTCTime)
-- import EulerHS.Prelude hiding (exp, id)

-- import Data.Text

import Beckn.Prelude
import Beckn.Utils.JSON (slashedRecordFields)
import Core.Descriptor
import Core.Provider (Provider)
import Data.Aeson

data Catalog = Catalog
  { bpp_descriptor :: Maybe DescriptorDetails,
    --       bpp_categories :: Maybe [Category],
    --       bpp_fulfillments :: Maybe [Fulfillment],
    --       bpp_payments :: Maybe [Payment],
    --       bpp_offers :: Maybe [Offer],
    bpp_providers :: Maybe [Provider]
    --       exp :: Maybe UTCTime
  }
  deriving (Generic, Show)

instance FromJSON Catalog where
  parseJSON = genericParseJSON slashedRecordFields

instance ToJSON Catalog where
  toJSON = genericToJSON slashedRecordFields {omitNothingFields = True}
