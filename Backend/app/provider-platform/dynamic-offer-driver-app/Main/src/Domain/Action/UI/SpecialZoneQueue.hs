{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.SpecialZoneQueue
  ( getSpecialZoneQueueRequest,
    postSpecialZoneQueueRequestRespond,
  )
where

import qualified API.Types.UI.SpecialZoneQueue
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.SpecialZoneQueueRequest
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common (Seconds (..))
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Queries.GateInfo as QGI
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator (AllocatorJobType (..), CheckPickupZoneArrivalJobData (..))
import SharedLogic.SpecialZoneDriverDemand (mkQueueSkipCountKey)
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.SpecialZoneQueueRequest as QSZQR
import Tools.Error

getSpecialZoneQueueRequest ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow API.Types.UI.SpecialZoneQueue.SpecialZoneQueueRequestListRes
  )
getSpecialZoneQueueRequest (mbPersonId, _merchantId, _merchantOpCityId) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person id")
  now <- getCurrentTime
  activeRequests <- QSZQR.findActiveByDriverId personId Domain.Types.SpecialZoneQueueRequest.Active
  validRequests <- collectValid now personId activeRequests
  pure $ API.Types.UI.SpecialZoneQueue.SpecialZoneQueueRequestListRes {requests = validRequests}
  where
    collectValid _ _ [] = pure []
    collectValid now personId (req : rest) =
      if req.validTill < now
        then do
          QSZQR.updateResponse (Just Domain.Types.SpecialZoneQueueRequest.Ignored) Domain.Types.SpecialZoneQueueRequest.Expired req.id
          collectValid now personId rest
        else do
          -- Get skip count from Redis
          mbSkipCount <- Redis.withCrossAppRedis $ Redis.get @Int (mkQueueSkipCountKey req.specialLocationId personId.getId)
          -- Get max skips from gate config
          mbGate <- Esq.runInReplica $ QGI.findById (Kernel.Types.Id.Id req.gateId)
          let maxSkips = mbGate >>= (.maxRideSkipsBeforeQueueRemoval)
          let res =
                API.Types.UI.SpecialZoneQueue.SpecialZoneQueueRequestRes
                  { requestId = req.id,
                    gateId = req.gateId,
                    gateName = req.gateName,
                    specialLocationId = req.specialLocationId,
                    specialLocationName = req.specialLocationName,
                    vehicleType = req.vehicleType,
                    validTill = req.validTill,
                    currentSkipCount = fromMaybe 0 mbSkipCount,
                    maxSkipsBeforeQueueRemoval = maxSkips
                  }
          rest' <- collectValid now personId rest
          pure (res : rest')

postSpecialZoneQueueRequestRespond ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest ->
    API.Types.UI.SpecialZoneQueue.SpecialZoneQueueRespondReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postSpecialZoneQueueRequestRespond (mbPersonId, _merchantId, _merchantOpCityId) requestId req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person id")
  request <- QSZQR.findByPrimaryKey requestId >>= fromMaybeM (InvalidRequest "Request not found")
  unless (request.driverId == personId) $ throwError (InvalidRequest "Request does not belong to this driver")
  unless (request.status == Domain.Types.SpecialZoneQueueRequest.Active) $ throwError (InvalidRequest "Request is no longer active")
  now <- getCurrentTime
  let actualResponse =
        if request.validTill < now
          then Domain.Types.SpecialZoneQueueRequest.Ignored
          else req.response
  QSZQR.updateResponse (Just actualResponse) Domain.Types.SpecialZoneQueueRequest.Expired requestId
  -- Schedule no-show check if driver accepted
  when (actualResponse == Domain.Types.SpecialZoneQueueRequest.Accept) $ do
    mbGate <- Esq.runInReplica $ QGI.findById (Kernel.Types.Id.Id request.gateId)
    let timeoutSec = maybe 1200 (fromMaybe 1200 . (.pickupZoneArrivalTimeoutInSec)) mbGate
    createJobIn @_ @'CheckPickupZoneArrival
      (Just request.merchantId)
      (Just request.merchantOperatingCityId)
      (secondsToNominalDiffTime $ Seconds timeoutSec)
      CheckPickupZoneArrivalJobData
        { requestId = requestId.getId,
          driverId = personId,
          gateId = request.gateId,
          specialLocationId = request.specialLocationId,
          vehicleType = request.vehicleType,
          merchantId = request.merchantId,
          merchantOperatingCityId = request.merchantOperatingCityId
        }
  pure Kernel.Types.APISuccess.Success
