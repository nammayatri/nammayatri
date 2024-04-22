module CAC.Config where

import Prelude
import CAC.Types
import Data.Maybe
import CAC.Customer.Types

getConfigFromCAC :: City -> Merchant -> CACConfig
getConfigFromCAC city merchant = case city of
  Bangalore ->
    defaultCityConfig
      { cityCode = Just "std:080"
      , cityName = Bangalore
      }
  Chennai ->
    defaultCityConfig
      { cityCode = Just "std:044"
      , cityName = Chennai
      }
  _ -> defaultCityConfig

defaultCityConfig :: CACConfig
defaultCityConfig =
  { cityCode: Nothing
  , cityName: AnyCity
  , geoCode:
      { radius: 50000
      , strictBounds: true
      }
  , enableCabs: false
  , estimateAndQuote:
      { showInfoIcon: true
      }
  , feature:
      { enableCabBanner: true
      , enableChangeRideVariant: true
      }
  }
