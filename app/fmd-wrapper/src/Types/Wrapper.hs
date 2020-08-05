module Types.Wrapper where

import Beckn.Types.Core.PaymentPolicy
import Beckn.Types.Core.Quotation
import Beckn.Types.FMD.Order
import EulerHS.Prelude
import External.Dunzo.Types (ClientId, ClientSecret)

data BAConfig = BAConfig
  { bap_nw_address :: Text,
    bap_id :: Text,
    bap_api_key :: Text,
    paymentPolicy :: PaymentPolicy
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- Newer integrations will be listed heres with their custom type
newtype WrapperClientConfig = Dunzo DunzoConfig
  deriving (Show, Generic, ToJSON, FromJSON)

data DunzoConfig = DunzoConfig
  { dzClientId :: ClientId,
    dzClientSecret :: ClientSecret,
    dzUrl :: Text,
    dzBAConfigs :: [BAConfig],
    dzBPId :: Text,
    dzBPNwAddress :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data OrderDetails = OrderDetails
  { order :: Order,
    quote :: Quotation
  }
  deriving (Show, Generic, ToJSON, FromJSON)
