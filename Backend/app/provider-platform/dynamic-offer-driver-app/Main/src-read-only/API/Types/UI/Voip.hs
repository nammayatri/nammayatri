{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Voip where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.Merchant
import qualified Domain.Types.Ride
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data UserType
  = DRIVER
  | RIDER
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VoipReq = VoipReq
  { callStatus :: VoipStatus,
    rideId :: Kernel.Types.Id.Id Domain.Types.Ride.Ride,
    errorCode :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    userType :: UserType,
    networkType :: Data.Text.Text,
    networkQuality :: Data.Text.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantCity :: Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VoipStatus
  = UNKNOWN_ERROR
  | SDK_NOT_INIT
  | MULTIPLE_VOIP_CALL_ATTEMPTS
  | NETWORK_ERROR
  | MIC_PERMISSION_DENIED
  | NO_INTERNET
  | CALL_CANCELLED_DUE_TO_RING_TIMEOUT
  | CALL_DECLINED
  | CALL_OVER
  | CALL_DECLINED_DUE_TO_NOTIFICATIONS_DISABLED
  | CALLEE_MICROPHONE_PERMISSION_BLOCKED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
