module Tools.Schema where

import Data.OpenApi
import qualified Tools.JSON as J

fareProductSchemaOptions :: SchemaOptions
fareProductSchemaOptions =
  defaultSchemaOptions
    { sumEncoding = J.fareProductTaggedObject,
      constructorTagModifier = J.fareProductConstructorModifier
    }
