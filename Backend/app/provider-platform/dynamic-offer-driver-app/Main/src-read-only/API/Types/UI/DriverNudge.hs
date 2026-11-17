{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.DriverNudge where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.DriverNudge
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Id
import Servant
import qualified SharedLogic.External.LocationTrackingService.Types
import Tools.Auth

data DemandNudgeResponseReq = DemandNudgeResponseReq {nudgeId :: Kernel.Types.Id.Id Domain.Types.DriverNudge.DriverNudge, status :: NudgeResponseStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NudgeBulkLocUpdateReq = NudgeBulkLocUpdateReq {driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person, loc :: [SharedLogic.External.LocationTrackingService.Types.LocationUpdate]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NudgeEndReq = NudgeEndReq {driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person, merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NudgeResponseStatus
  = ACCEPTED
  | DECLINED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NudgeStartReq = NudgeStartReq {driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person, merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
