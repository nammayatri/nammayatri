{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.SpecialZoneQueue where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.SpecialZoneQueueRequest
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data SpecialZoneQueueRequestListRes = SpecialZoneQueueRequestListRes {currentSkipCount :: Kernel.Prelude.Int, maxSkipsBeforeQueueRemoval :: Kernel.Prelude.Maybe Kernel.Prelude.Int, requests :: [SpecialZoneQueueRequestRes]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SpecialZoneQueueRequestRes = SpecialZoneQueueRequestRes
  { gateId :: Kernel.Prelude.Text,
    gateName :: Kernel.Prelude.Text,
    requestId :: Kernel.Types.Id.Id Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest,
    specialLocationId :: Kernel.Prelude.Text,
    specialLocationName :: Kernel.Prelude.Text,
    status :: Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequestStatus,
    validTill :: Kernel.Prelude.UTCTime,
    vehicleType :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SpecialZoneQueueRespondReq = SpecialZoneQueueRespondReq {response :: Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequestResponse}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
