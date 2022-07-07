module Domain.Types.Merchant where

import Beckn.Prelude
import Beckn.Types.Id

data Merchant = Merchant
  { id :: Id Merchant,
    shortId :: ShortId Merchant
  }
  deriving (Generic)
