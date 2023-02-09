module Tools.JSON where

import Data.Aeson
import Kernel.Prelude

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
  "OneWayDetailsAPIEntity" -> "ONE_WAY"
  "RentalDetailsAPIEntity" -> "RENTAL"
  x -> x
