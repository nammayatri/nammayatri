-- module Beckn.Types.Core.Migration.Catalog (Catalog (..)) where

module Core.Catalog where

-- import Beckn.Types.Core.Migration.Category (Category)
import Core.Descriptor 
import Beckn.Types.Core.Migration.Image (Image (..))
-- import Beckn.Types.Core.Migration.Fulfillment (Fulfillment)
-- import Beckn.Types.Core.Migration.Offer (Offer)
-- import Beckn.Types.Core.Migration.Payment (Payment)
import Core.Provider (Provider)
import Beckn.Utils.Example
import Beckn.Utils.JSON (slashedRecordFields)
-- import Data.Time (UTCTime)
-- import EulerHS.Prelude hiding (exp, id)

import Data.Aeson
-- import Data.Text
import GHC.Generics   

data Catalog = Catalog
  { bpp_descriptor :: Maybe Descriptor,
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
  toJSON = genericToJSON slashedRecordFields { omitNothingFields = True }

instance Example Catalog where
  example = 
    Catalog
      { bpp_descriptor = Just emptyDescriptor {name = Just "State Water Transport Department",
                                               code = Just "SWTD",
                                               symbol  = Just "string",
                                               short_desc  = Just "string",
                                               long_desc  = Just "string",
                                               images = Just ([Image "string"])
                                              },
                                            
        --          bpp_categories
        --          bpp_fulfillments = Nothing,
        --          bpp_payments = Nothing,
        --          bpp_offers = Nothing,
        bpp_providers = Just [example]
        --          exp = Nothing
      }

example_catalog :: Catalog
example_catalog = example
