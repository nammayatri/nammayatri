module Types.API.Status where

import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Mobility.Service
import EulerHS.Prelude

newtype StatusReq = StatusReq
  { productInstanceId :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type StatusRes = AckResponse
