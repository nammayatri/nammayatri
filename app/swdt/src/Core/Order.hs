-- module Beckn.Types.Core.Migration.Order where
module Core.Order where

--         import Beckn.Types.Common (IdObject)
--         import Beckn.Types.Core.Migration.Billing
-- import Beckn.Types.Core.Migration.Fulfillment (Fulfillment)
-- import Beckn.Types.Core.Migration.ItemQuantity
-- import Beckn.Types.Core.Migration.Payment
import Core.Fulfillment
import Core.Item
import Core.Payment
import Core.Provider
import Core.Quotation (Quotation (..))
import Beckn.Utils.Example


import Data.Aeson
import Data.Text
import GHC.Generics

-- import EulerHS.Prelude hiding (State, id, state)

data Order = Order
  { id :: Maybe Text,
    state:: Maybe State,
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
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

data State
  = Active
  | CANCELLED
  deriving (Generic, ToJSON, FromJSON, Show)

data Billing =Billing
  {billing :: Text
  } 
  deriving (Generic, ToJSON, FromJSON, Show)


instance Example Order where
  example =
    Order
      { id = Just "ORDER_1",
        state = Just CANCELLED,
        provider = Just (Provider (Just "KMRL") Nothing Nothing Nothing Nothing),
        items = [Item (Just "ONE_WAY_TICKET") (Just "TRIP001_EKM_EMB") Nothing Nothing (Just (Quantity 1))],
        -- add_ons = [],
        -- offers = [],
        billing = Billing "User Name", 
        fulfillment = Fulfillment 
                        (Just "TRIP001_EKM_EMB") Nothing Nothing,
                        
        quote = Just example,
        payment = example
      }

example_order :: Order
example_order = example

--      newtype OrderObject = OrderObject
--        { order :: Order
--        }
--        deriving (Generic, Show, FromJSON, ToJSON)
