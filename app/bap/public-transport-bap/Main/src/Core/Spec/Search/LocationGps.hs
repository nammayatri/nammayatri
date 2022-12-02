module Core.Spec.Search.LocationGps where

import Beckn.Types.Core.Gps
import Beckn.Utils.GenericPretty (PrettyShow)
import Beckn.Utils.Schema
import Data.Aeson
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)
import Relude hiding (id)

newtype LocationGps = LocationGps {gps :: Gps}
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, PrettyShow)

instance ToSchema LocationGps where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
