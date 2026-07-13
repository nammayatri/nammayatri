module Domain.Types.Entity where

import qualified Domain.Types.Merchant as DMerchant
import Kernel.Prelude
import Kernel.Types.Id

data Entity = Entity
  { id :: Id Entity,
    merchantId :: Id DMerchant.Merchant,
    entityName :: Text,
    entityShortId :: ShortId Entity,
    deleted :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
