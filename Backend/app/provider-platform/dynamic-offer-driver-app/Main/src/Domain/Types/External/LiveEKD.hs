module Domain.Types.External.LiveEKD where

import EulerHS.Prelude
import Kernel.Prelude
import Kernel.Utils.Dhall

data VocalyticsCnfg = VocalyticsCnfg
  { url :: BaseUrl,
    token :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, FromDhall)

data LiveEKDRequest = LiveEKDRequest
  { file :: Text,
    call_id :: Text,
    user_type :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq)

data LiveEKDResponse = LiveEKDResponse {}
  deriving (Generic, FromJSON, ToJSON, Show, Eq)
