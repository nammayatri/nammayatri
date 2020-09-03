module Types.Wrapper where

import Beckn.Types.App
import Beckn.Types.Core.PaymentEndpoint
import Beckn.Types.Core.PaymentPolicy
import Beckn.Types.Core.Quotation
import Beckn.Types.FMD.Order
import EulerHS.Prelude
import External.Dunzo.Types (ClientId, ClientSecret)

data BAConfig = BAConfig
  { bap_nw_address :: BaseUrl,
    bap_id :: Text,
    bap_api_key :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- Newer integrations will be listed heres with their custom type
newtype WrapperClientConfig = Dunzo DunzoConfig
  deriving (Show, Generic, ToJSON, FromJSON)

data DunzoConfig = DunzoConfig
  { dzClientId :: ClientId,
    dzClientSecret :: ClientSecret,
    dzUrl :: BaseUrl,
    dzBAConfigs :: [BAConfig],
    dzBPId :: Text,
    dzBPNwAddress :: BaseUrl,
    paymentPolicy :: PaymentPolicy,
    payee :: PaymentEndpoint
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data OrderDetails = OrderDetails
  { order :: Order,
    quote :: Quotation
  }
  deriving (Show, Generic, ToJSON, FromJSON)
