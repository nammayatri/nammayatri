module Beckn.Types.Core.Taxi.Common.FareProductType where

import Beckn.Prelude hiding (show)

-- TODO
-- ONE_WAY_TRIP, RENTAL_TRIP, DRIVER_OFFER_ESTIMATE can be used in on_search,
-- only DRIVER_OFFER can be used in on_select,
-- ONE_WAY_TRIP, RENTAL_TRIP, DRIVER_OFFER can be used in init
-- we can express it in specific types for each endpoint
data FareProductType
  = ONE_WAY_TRIP
  | RENTAL_TRIP
  | DRIVER_OFFER_ESTIMATE
  | DRIVER_OFFER
  deriving
    ( Eq,
      Ord,
      Generic,
      ToSchema,
      Show,
      FromJSON,
      ToJSON,
      Read
    )
