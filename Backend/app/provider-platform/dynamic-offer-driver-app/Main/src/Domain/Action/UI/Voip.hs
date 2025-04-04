{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.Voip (postCallVoip) where

-- import qualified API.Types.UI.Voip as Voip

-- import qualified Lib.Utils.Storage.Beam.VoipCallStatus as BeamVCS

import qualified Control.Monad.IO.Class as M
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant as DMC
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person
import Domain.Types.Ride as Ride
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Lib.Utils.Storage.Beam.BeamFlow ()
import qualified Lib.Utils.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Utils.Storage.Queries.VoipCallStatusExtra as QVoip
import qualified Lib.Utils.Types.VoipCallStatus as VCS
import Servant
import Storage.Beam.VoipCallStatus ()
import Tools.Auth
import Utils.Common.Voip.Types.VoipApiType as Voip

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
            VCS.callId = Just req.callId,
            VCS.callStatus = req.callStatus,
            VCS.rideId = Kernel.Types.Id.cast req.rideId,
            VCS.errorCode = req.errorCode,
            VCS.networkType = req.networkType,
            VCS.networkQuality = req.networkQuality,
            VCS.merchantId = Kernel.Types.Id.cast req.merchantId,
            VCS.merchantCity = Kernel.Types.Id.cast req.merchantCity,
            VCS.createdAt = now,
            VCS.updatedAt = now
          }
  QVoip.upsert voipCallData
  return Success
