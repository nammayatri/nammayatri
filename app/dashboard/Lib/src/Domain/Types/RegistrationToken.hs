module Domain.Types.RegistrationToken where

import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person.Type as DP
import Kernel.Prelude
import Kernel.Types.Id

data RegistrationToken = RegistrationToken
  { id :: Id RegistrationToken,
    token :: Text,
    personId :: Id DP.Person,
    createdAt :: UTCTime,
    merchantId :: Id DMerchant.Merchant
  }
  deriving (Generic, Show)
