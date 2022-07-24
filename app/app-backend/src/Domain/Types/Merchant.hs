module Domain.Types.Merchant where

import Beckn.Prelude
import Beckn.Types.Id

data Merchant = Merchant
  { id :: Id Merchant,
    shortId :: ShortId Merchant,
    exoPhone :: Maybe Text,
    exoPhoneCountryCode :: Maybe Text
  }
  deriving (Generic)
