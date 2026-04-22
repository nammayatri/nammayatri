module Domain.Types.External.Aarokya where

import EulerHS.Prelude
import Kernel.Prelude

data AarokyaTokenRequest = AarokyaTokenRequest
  { phone_country_code :: Text,
    phone_number :: Text,
    platform_id :: Text,
    dl_number :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq)

data AarokyaTokenResponse = AarokyaTokenResponse
  { access_token :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq)
