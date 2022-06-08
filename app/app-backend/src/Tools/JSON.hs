module Tools.JSON (taggedValueOptions, taggedValueSchemaOptions) where

import Beckn.Prelude
import Data.Aeson
import qualified Data.OpenApi as OpenApi

taggedValueOptions :: Options
taggedValueOptions =
  defaultOptions
    { sumEncoding = taggedObject,
      constructorTagModifier = constructorModifier
    }

taggedValueSchemaOptions :: OpenApi.SchemaOptions
taggedValueSchemaOptions =
  OpenApi.defaultSchemaOptions
    { OpenApi.sumEncoding = taggedObject,
      OpenApi.constructorTagModifier = constructorModifier
    }

taggedObject :: SumEncoding
taggedObject =
  defaultTaggedObject
    { tagFieldName = "fareProductType"
    }

constructorModifier :: String -> String
constructorModifier = \case
  "OneWayDetails" -> "ONE_WAY"
  "RentalDetails" -> "RENTAL"
  "OneWaySearch" -> "ONE_WAY"
  "RentalSearch" -> "RENTAL"
  x -> x
