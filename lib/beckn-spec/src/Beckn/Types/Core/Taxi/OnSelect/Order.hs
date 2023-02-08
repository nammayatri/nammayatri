module Beckn.Types.Core.Taxi.OnSelect.Order where

import Beckn.Types.Core.Taxi.OnSelect.Provider
import Data.OpenApi (ToSchema (..), fromAesonOptions)
import Kernel.Prelude
import Kernel.Utils.JSON (slashedRecordFields)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

newtype Order = Order
  { provider :: Provider
  }
  deriving (Generic, Show)

instance ToSchema Order where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions slashedRecordFields

instance FromJSON Order where
  parseJSON = genericParseJSON slashedRecordFields

instance ToJSON Order where
  toJSON = genericToJSON slashedRecordFields
