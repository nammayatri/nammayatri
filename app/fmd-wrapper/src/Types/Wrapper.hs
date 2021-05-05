module Types.Wrapper where

import Beckn.Types.App
import Beckn.Utils.Dhall (FromDhall)
import EulerHS.Prelude
import Types.Beckn.Domain.Order
import Types.Beckn.Quotation
import Types.Common

-- BAP with Dunzo account will have these details
-- in `organization.info`
data DzBAConfig = DzBAConfig
  { bapId :: Text,
    dzClientId :: ClientId,
    dzClientSecret :: ClientSecret
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data DunzoConfig = DunzoConfig
  { dzUrl :: BaseUrl,
    dzTokenUrl :: BaseUrl,
    dzBPId :: Text,
    dzBPNwAddress :: BaseUrl,
    payee :: Text,
    dzTestMode :: Bool,
    dzQuotationTTLinMin :: Integer
  }
  deriving (Show, Generic, ToJSON, FromJSON, FromDhall)

data OrderDetails = OrderDetails
  { order :: Order,
    quote :: Quotation
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data DlBAConfig = DlBAConfig
  { bapId :: Text,
    dlClientId :: ClientId,
    dlClientSecret :: ClientSecret
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data DelhiveryConfig = DelhiveryConfig
  { dlUrl :: BaseUrl,
    dlTokenUrl :: BaseUrl,
    dlBPId :: Text,
    dlBPNwAddress :: BaseUrl,
    dlPayee :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, FromDhall)
