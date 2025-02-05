{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.Voip (postCallVoip) where

import qualified API.Types.UI.Voip as Voip
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant as DMC
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person
import Domain.Types.Ride as Ride
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Lib.Utils (pushToKafka)
import Kernel.Prelude
import Kernel.Types.APISuccess
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import Kernel.Types.GuidLike
import qualified Kernel.Types.Id
import Kernel.Utils.Logging (logDebug)
import Servant
import Tools.Auth

data VOIPData = VOIPData
  { callId :: Text,
    callStatus :: Voip.VoipStatus,
    rideId :: Kernel.Types.Id.Id Ride.Ride,
    errorCode :: Maybe Int,
    userType :: Voip.UserType,
    networkType :: Text,
    networkQuality :: Text,
    merchantId :: Kernel.Types.Id.Id DMC.Merchant,
    merchantCity :: Text,
    personId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    createdAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON)

postCallVoip ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id DMC.Merchant,
      Kernel.Types.Id.Id DMOC.MerchantOperatingCity
    ) ->
    Voip.VoipReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postCallVoip (personId, _, _mid) req = do
  now <- getCurrentTime
  uid <- generateGUID
  let voipCallData =
        VOIPData
          { callId = uid,
            callStatus = req.callStatus,
            rideId = req.rideId,
            errorCode = req.errorCode,
            userType = req.userType,
            networkType = req.networkType,
            networkQuality = req.networkQuality,
            merchantId = req.merchantId,
            merchantCity = req.merchantCity,
            personId = personId,
            createdAt = now
          }
  pushToKafka voipCallData "voip-call-data" voipCallData.rideId.getId
  return Success
