module CAC.Types where

import Prelude
import Data.Show.Generic (genericShow)
import Data.Generic.Rep (class Generic)
import Data.Maybe

data City
  = Bangalore
  | Kolkata
  | Paris
  | Kochi
  | Delhi
  | Hyderabad
  | Mumbai
  | Chennai
  | Coimbatore
  | Pondicherry
  | Goa
  | Pune
  | Mysore
  | Tumakuru
  | AnyCity

derive instance genericCity :: Generic City _
instance showCity :: Show City where
  show = genericShow

data Merchant
  = NammaYatri
  | ManaYatri
  | Yatri
  | YatriSathi
  | Bridge
  | Mobility_PM
  | Mobility_RS
  | AnyMerchant

derive instance genericMerchant :: Generic Merchant _
instance showMerchant :: Show Merchant where
  show = genericShow

type Context
  = String
