module Types.Wrapper where

import Beckn.Types.App
import Beckn.Utils.Dhall (FromDhall)
import EulerHS.Prelude

data DunzoConfig = DunzoConfig
  { dzUrl :: BaseUrl,
    dzTokenUrl :: BaseUrl,
    payee :: Text,
    dzTestMode :: Bool,
    dzQuotationTTLinMin :: Integer,
    dzCredsId :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, FromDhall)
