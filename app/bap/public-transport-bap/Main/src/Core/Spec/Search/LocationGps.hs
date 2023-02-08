module Core.Spec.Search.LocationGps where

import Data.Aeson
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)
import Kernel.Types.Beckn.Gps
import Kernel.Utils.GenericPretty (PrettyShow)
import Kernel.Utils.Schema
import Relude hiding (id)

newtype LocationGps = LocationGps {gps :: Gps}
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, PrettyShow)

instance ToSchema LocationGps where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
