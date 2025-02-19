{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.Voip (postCallVoip) where

import qualified API.Types.UI.Voip as Voip
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant as DMC
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person
import Domain.Types.Ride as Ride
-- import qualified Kernel.Types.APISuccess

import qualified Domain.Types.VoipCallStatus as VCS
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Lib.Utils (pushToKafka)
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Common
import Kernel.Types.GuidLike
import qualified Kernel.Types.Id
import Kernel.Utils.Logging (logDebug)
import Servant
import qualified Storage.Queries.VoipCallStatusExtra as QVoip
import Tools.Auth

postCallVoip ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id DMC.Merchant,
      Kernel.Types.Id.Id DMOC.MerchantOperatingCity
    ) ->
    Voip.VoipReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postCallVoip (_pid, _mid, _mcid) req = do
  now <- getCurrentTime
  uid <- generateGUID
  let voipCallData =
        VCS.VoipCallStatus
          { VCS.id = uid,
            VCS.callId = req.callId,
            VCS.callStatus = convertVoipStatus (getField @"callStatus" req),
            VCS.rideId = req.rideId,
            VCS.errorCode = req.errorCode,
            VCS.networkType = req.networkType,
            VCS.networkQuality = req.networkQuality,
            VCS.merchantId = req.merchantId,
            VCS.merchantOperatingCityId = Just (req.merchantCityId),
            VCS.createdAt = now,
            VCS.updatedAt = now
          }
  QVoip.upsert voipCallData
  return Success

convertVoipStatus :: Voip.VoipStatus -> VCS.VoipStatus
convertVoipStatus Voip.CALL_IS_PLACED = VCS.CALL_IS_PLACED
convertVoipStatus Voip.CALL_RINGING = VCS.CALL_RINGING
convertVoipStatus Voip.CALL_MISSED = VCS.CALL_MISSED
convertVoipStatus Voip.CALL_CANCELLED = VCS.CALL_CANCELLED
convertVoipStatus Voip.CALL_DECLINED_DUE_TO_BUSY_ON_PSTN = VCS.CALL_DECLINED_DUE_TO_BUSY_ON_PSTN
convertVoipStatus Voip.CALL_DECLINED_DUE_TO_BUSY_ON_VOIP = VCS.CALL_DECLINED_DUE_TO_BUSY_ON_VOIP
convertVoipStatus Voip.CALL_OVER_DUE_TO_NETWORK_DELAY_IN_MEDIA_SETUP = VCS.CALL_OVER_DUE_TO_NETWORK_DELAY_IN_MEDIA_SETUP
convertVoipStatus Voip.CALL_OVER_DUE_TO_PROTOCOL_MISMATCH = VCS.CALL_OVER_DUE_TO_PROTOCOL_MISMATCH
convertVoipStatus Voip.CALL_OVER_DUE_TO_REMOTE_NETWORK_LOSS = VCS.CALL_OVER_DUE_TO_REMOTE_NETWORK_LOSS
convertVoipStatus Voip.CALL_OVER_DUE_TO_LOCAL_NETWORK_LOSS = VCS.CALL_OVER_DUE_TO_LOCAL_NETWORK_LOSS
convertVoipStatus Voip.CALL_IN_PROGRESS = VCS.CALL_IN_PROGRESS
convertVoipStatus Voip.CALL_ANSWERED = VCS.CALL_ANSWERED
convertVoipStatus Voip.CALL_CANCELLED_DUE_TO_RING_TIMEOUT = VCS.CALL_CANCELLED_DUE_TO_RING_TIMEOUT
convertVoipStatus Voip.CALL_DECLINED = VCS.CALL_DECLINED
convertVoipStatus Voip.CALL_OVER = VCS.CALL_OVER
convertVoipStatus Voip.CALL_DECLINED_DUE_TO_LOGGED_OUT_CUID = VCS.CALL_DECLINED_DUE_TO_LOGGED_OUT_CUID
convertVoipStatus Voip.CALL_DECLINED_DUE_TO_NOTIFICATIONS_DISABLED = VCS.CALL_DECLINED_DUE_TO_NOTIFICATIONS_DISABLED
convertVoipStatus Voip.CALLEE_MICROPHONE_PERMISSION_BLOCKED = VCS.CALLEE_MICROPHONE_PERMISSION_BLOCKED
convertVoipStatus Voip.CALL_FAILED_DUE_TO_INTERNAL_ERROR = VCS.CALL_FAILED_DUE_TO_INTERNAL_ERROR
convertVoipStatus Voip.MULTIPLE_VOIP_CALL_ATTEMPTS = VCS.MULTIPLE_VOIP_CALL_ATTEMPTS
convertVoipStatus Voip.MIC_PERMISSION_DENIED = VCS.MIC_PERMISSION_DENIED
convertVoipStatus Voip.NETWORK_ERROR = VCS.NETWORK_ERROR
convertVoipStatus Voip.NO_INTERNET = VCS.NO_INTERNET
convertVoipStatus Voip.SDK_NOT_INIT = VCS.SDK_NOT_INIT
convertVoipStatus Voip.UNKNOWN_ERROR = VCS.UNKNOWN_ERROR
