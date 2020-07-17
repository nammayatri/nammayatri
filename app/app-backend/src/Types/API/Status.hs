module Types.API.Status where

import Beckn.Types.Common
import EulerHS.Prelude

newtype StatusReq = StatusReq
  { productInstanceId :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type StatusRes = AckResponse
