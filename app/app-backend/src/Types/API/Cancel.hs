module Types.API.Cancel where

import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Mobility.Service
import EulerHS.Prelude

data Entity = CASE | PRODUCT
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

data Cancel = Cancel
  { entityId :: Text,
    entityType :: Entity
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data CancelReq = CancelReq
  { context :: Context,
    message :: Cancel
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type CancelRes = AckResponse
