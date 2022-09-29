module Idfy.Types.IdfyConfig where

import Beckn.Types.App
import Beckn.Utils.Dhall (FromDhall)
import Data.Aeson hiding (Error)
import Data.OpenApi hiding (url)
import EulerHS.Prelude hiding (state)

data IdfyConfig = IdfyConfig
  { account_id :: AccountId,
    api_key :: ApiKey,
    secret :: Text,
    url :: BaseUrl
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema, FromDhall)

type AccountId = Text

type ApiKey = Text

data StatusCheck = VALID | INVALID deriving (Generic, ToJSON, Show, FromJSON, ToSchema)
