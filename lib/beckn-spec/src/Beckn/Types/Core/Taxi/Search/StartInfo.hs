module Beckn.Types.Core.Taxi.Search.StartInfo
  ( module Beckn.Types.Core.Taxi.Search.StartInfo,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.TimeTimestamp as Reexport
import Beckn.Types.Core.Taxi.Search.Location (Location)
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
