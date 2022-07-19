module Beckn.Types.Core.Taxi.Common.FareProductType where

import Beckn.Prelude hiding (show)

data FareProductType
  = ONE_WAY_TRIP
  | RENTAL_TRIP
  | AUTO_TRIP -- FIXME: I think we don't need this (_Yuri_)
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
