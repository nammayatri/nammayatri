{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Catalog (Catalog (..)) where

import Beckn.Types.Core.Migration.Category (Category)
import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Fulfillment (Fulfillment)
import Beckn.Types.Core.Migration.Offer (Offer)
import Beckn.Types.Core.Migration.Payment (Payment)
import Beckn.Types.Core.Migration.Provider (Provider)
import Beckn.Types.Core.Migration.ProviderCatalog (ProviderCatalog)
import Beckn.Utils.JSON (deriveJSON, uniteObjects)
import Data.Aeson (Options (..), defaultOptions)
import Data.Text (pack, replace, unpack)
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (pack, unpack)

data Catalog = Catalog
  { _bpp_descriptor :: Maybe Descriptor,
    _bpp_categories :: [Category],
    _bpp_fulfillments :: [Fulfillment],
    _bpp_payments :: [Payment],
    _bpp_offers :: [Offer],
    _bpp_providers :: [BppProvider],
    _exp :: Maybe UTCTime
  }
  deriving (Generic)

-- allOf union
data BppProvider = BppProvider Provider ProviderCatalog

instance FromJSON BppProvider where
  parseJSON v = BppProvider <$> parseJSON v <*> parseJSON v

instance ToJSON BppProvider where
  toJSON (BppProvider p pc) = uniteObjects [toJSON p, toJSON pc]

slashedRecordFields :: Options
slashedRecordFields =
  defaultOptions
    { fieldLabelModifier = \('_' : xs) -> unpack . replace "_" "/" . pack $ xs
    }

deriveJSON ''Catalog 'slashedRecordFields
