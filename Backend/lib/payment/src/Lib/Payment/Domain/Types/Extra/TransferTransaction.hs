module Lib.Payment.Domain.Types.Extra.TransferTransaction where

import Data.Aeson.Types
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import Kernel.Prelude
import Kernel.Storage.Esqueleto (derivePersistField)

-- Extra code goes here --
data TransferEntity

data TransferEntityName = CASH_RIDES_COMMISSION
  deriving (Show, Read, Eq, Ord, Generic)

-- Generic instances for type with single value will not work
instance FromJSON TransferEntityName where
  parseJSON (String "CASH_RIDES_COMMISSION") = pure CASH_RIDES_COMMISSION
  parseJSON e = typeMismatch "String" e

instance ToJSON TransferEntityName where
  toJSON = String . show

$(mkBeamInstancesForEnum ''TransferEntityName)

derivePersistField "TransferEntityName"
