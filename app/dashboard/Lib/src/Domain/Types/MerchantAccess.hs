module Domain.Types.MerchantAccess where

import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DPerson
import Kernel.Prelude
import Kernel.Types.Id

data MerchantAccess = MerchantAccess
  { id :: Id MerchantAccess,
    merchantId :: Id DMerchant.Merchant,
    personId :: Id DPerson.Person,
    createdAt :: UTCTime
  }
  deriving (Generic, Show)
