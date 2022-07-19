module Beckn.Types.Core.Taxi.OnSelect.StartInfo
  ( module Beckn.Types.Core.Taxi.OnSelect.StartInfo,
    module Reexport,
  )
where

-- FIXME Reuse from Beckn.Types.Core.Taxi.Search.StartInfo when spec changes will be merged
import Beckn.Types.Core.Taxi.Common.TimeTimestamp as Reexport
import Beckn.Types.Core.Taxi.OnSelect.Location (Location)
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (id)

data StartInfo = StartInfo
  { location :: Location,
    time :: TimeTimestamp
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema StartInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
