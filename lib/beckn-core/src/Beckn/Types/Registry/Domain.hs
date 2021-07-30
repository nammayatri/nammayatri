module Beckn.Types.Registry.Domain (Domain (..)) where

import Beckn.Utils.JSON (constructorsWithHyphens)
import Data.Aeson
import EulerHS.Prelude

data Domain
  = MOBILITY
  | FINAL_MILE_DELIVERY
  | LOCAL_RETAIL
  | FOOD_AND_BEVERAGE
  | HEALTHCARE
  deriving (Eq, Generic, Show)

instance ToJSON Domain where
  toJSON LOCAL_RETAIL = String "nic2004:52110"
  toJSON FINAL_MILE_DELIVERY = String "nic2004:55204"
  toJSON val = genericToJSON constructorsWithHyphens val -- TODO: update remaining domains with codes

instance FromJSON Domain where
  parseJSON (String "nic2004:52110") = pure LOCAL_RETAIL
  parseJSON (String "nic2004:55204") = pure FINAL_MILE_DELIVERY
  parseJSON val = genericParseJSON constructorsWithHyphens val
