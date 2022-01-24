module Core.Order where

import Beckn.Prelude
import Core.Fulfillment
import Core.OnSearch.Item
import Core.Payment
import Core.Provider
import Core.Quotation (Quotation (..))
import Data.Aeson

data Order = Order
  { id :: Maybe Text,
    state :: Maybe State,
    provider :: Maybe Provider,
    items :: [Item],
    -- add_ons :: [IdObject],
    -- offers :: [IdObject],
    billing :: Billing,
    fulfillment :: Fulfillment,
    quote :: Maybe Quotation,
    payment :: Maybe Payment
  }
  deriving (Generic, FromJSON, Show)

instance ToJSON Order where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

data State
  = Active
  | CANCELLED
  deriving (Generic, ToJSON, FromJSON, Show)

newtype Billing = Billing
  { billing :: Text
  }
  deriving (Generic, ToJSON, FromJSON, Show)
