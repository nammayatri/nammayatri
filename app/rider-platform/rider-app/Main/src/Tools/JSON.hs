module Tools.JSON where

import Data.Aeson
import Kernel.Prelude

-- FIXME: make generic instances more powerful to capture this case
fareProductOptions :: Options
fareProductOptions =
  defaultOptions
    { sumEncoding = fareProductTaggedObject,
      constructorTagModifier = fareProductConstructorModifier
    }

fareProductTaggedObject :: SumEncoding
fareProductTaggedObject =
  defaultTaggedObject
    { tagFieldName = "fareProductType"
    }

fareProductConstructorModifier :: String -> String
fareProductConstructorModifier = \case
  "OneWayAPIDetails" -> "ONE_WAY"
  "RentalAPIDetails" -> "RENTAL"
  "DriverOfferAPIDetails" -> "DRIVER_OFFER"
  x -> x
