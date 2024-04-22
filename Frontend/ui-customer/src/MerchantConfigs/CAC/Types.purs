module CAC.Customer.Types where

import CAC.Types
import Data.Maybe

type CACConfig
  = { cityName :: City
    , cityCode :: Maybe String
    , geoCode :: GeoCodeConfig
    , enableCabs :: Boolean
    , estimateAndQuote :: EstimateConfig
    , feature :: Features
    }

type GeoCodeConfig
  = { radius :: Int
    , strictBounds :: Boolean
    }

type EstimateConfig
  = { showInfoIcon :: Boolean
    }

type Features
  = { enableCabBanner :: Boolean
    , enableChangeRideVariant :: Boolean
    }
