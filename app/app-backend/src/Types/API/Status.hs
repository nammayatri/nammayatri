module Types.API.Status where

import Beckn.Types.APISuccess (APISuccess)
import EulerHS.Prelude

newtype StatusReq = StatusReq
  { productInstanceId :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type StatusRes = APISuccess
