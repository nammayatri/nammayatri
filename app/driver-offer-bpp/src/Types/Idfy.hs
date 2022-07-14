 module Types.Idfy where

import Beckn.Types.App
import Beckn.Utils.Dhall (FromDhall)
import EulerHS.Prelude

data IdfyConfig = IdfyConfig
  { idfyUrl :: BaseUrl,
    idfyAccountId :: Text,
    idfyApiKey :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, FromDhall)
