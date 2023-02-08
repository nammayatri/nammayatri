module Beckn.Types.Core.Taxi.OnConfirm.StartInfo
  ( module Beckn.Types.Core.Taxi.OnConfirm.StartInfo,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.TimeTimestamp as Reexport
import Beckn.Types.Core.Taxi.OnConfirm.Location (Location)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data StartInfo = StartInfo
  { time :: TimeTimestamp,
    location :: Location
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema StartInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
