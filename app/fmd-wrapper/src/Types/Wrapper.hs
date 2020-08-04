module Types.Wrapper where

import Beckn.Types.Core.PaymentPolicy
import EulerHS.Prelude
import External.Dunzo.Types (ClientId, ClientSecret)

data BAConfig = BAConfig
  { bap_nw_address :: Text,
    bap_id :: Text,
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
