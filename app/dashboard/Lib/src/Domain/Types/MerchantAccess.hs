module Domain.Types.MerchantAccess where

import Beckn.Prelude
import Beckn.Types.Id
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DPerson

data MerchantAccess = MerchantAccess
  { id :: Id MerchantAccess,
    merchantId :: Id DMerchant.Merchant,
    personId :: Id DPerson.Person,
    createdAt :: UTCTime
  }
  deriving (Generic, Show)
