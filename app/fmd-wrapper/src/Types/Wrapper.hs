module Types.Wrapper where

import Beckn.Types.Core.PaymentPolicy
import EulerHS.Prelude
import External.Dunzo.Types (ClientId, ClientSecret)

data BAConfig = BAConfig
  { bap_nw_address :: Text,
    paymentPolicy :: PaymentPolicy
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data WrapperClientConfig = Dunzo
  { dzClientId :: ClientId,
    dzClientSecret :: ClientSecret,
    dzBAConfigs :: [BAConfig]
  }
  deriving (Show, Generic, ToJSON, FromJSON)
