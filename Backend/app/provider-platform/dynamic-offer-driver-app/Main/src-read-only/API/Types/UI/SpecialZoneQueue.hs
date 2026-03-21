{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.SpecialZoneQueue where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.SpecialZoneQueueRequest
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data SpecialZoneQueueRequestListRes = SpecialZoneQueueRequestListRes {requests :: [SpecialZoneQueueRequestRes]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SpecialZoneQueueRequestRes = SpecialZoneQueueRequestRes
  { gateName :: Kernel.Prelude.Text,
    requestId :: Kernel.Types.Id.Id Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest,
    specialLocationName :: Kernel.Prelude.Text,
    validTill :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SpecialZoneQueueRespondReq = SpecialZoneQueueRespondReq {response :: Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequestResponse}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
