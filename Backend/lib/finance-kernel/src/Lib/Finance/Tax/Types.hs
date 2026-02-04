module Lib.Finance.Tax.Types
  ( TaxBreakdown (..),
    TaxComponent (..),
    TaxableItem (..),
    TaxJurisdiction (..),
  )
where

import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)

-- | Tax jurisdiction
data TaxJurisdiction = TaxJurisdiction
  { countryCode :: Text, -- ISO 3166: "IN", "US", "AE"
    stateCode :: Maybe Text, -- "KA", "CA"
    cityCode :: Maybe Text -- Optional city level
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Single tax component
data TaxComponent = TaxComponent
  { componentType :: Text, -- "cgst", "sgst", "igst", "vat", "sales_tax"
    rate :: Double, -- e.g., 0.09 for 9%
    taxAmount :: HighPrecMoney
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Complete tax breakdown
data TaxBreakdown = TaxBreakdown
  { taxableAmount :: HighPrecMoney,
    components :: [TaxComponent],
    totalTax :: HighPrecMoney
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Item to calculate tax for
data TaxableItem = TaxableItem
  { itemType :: Text, -- Domain provides: "ride_fare", "commission", "partner_fee"
    itemAmount :: HighPrecMoney,
    jurisdiction :: TaxJurisdiction
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
