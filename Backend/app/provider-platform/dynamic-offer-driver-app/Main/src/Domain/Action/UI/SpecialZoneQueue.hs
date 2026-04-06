{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.SpecialZoneQueue
  ( getSpecialZoneQueueRequest,
    postSpecialZoneQueueRequestRespond,
    postSpecialZoneQueueRequestCancel,
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
  -- Fetch both Active and Accepted requests
  activeRequests <- QSZQR.findActiveByDriverId personId Domain.Types.SpecialZoneQueueRequest.Active
  acceptedRequests <- QSZQR.findActiveByDriverId personId Domain.Types.SpecialZoneQueueRequest.Accepted
  -- Lazy-expire Active requests past validTill
  validActiveRequests <- collectValidActive now activeRequests
  -- Lazy no-show check for Accepted requests past arrival timeout (fallback if scheduler job is delayed)
  validAcceptedRequests <- collectValidAccepted now acceptedRequests
  let allRequests = validActiveRequests ++ validAcceptedRequests
  -- Get skip count
  (skipCount, maxSkips) <- case allRequests of
    (r : _) -> do
      mbSkipCount <- Redis.withCrossAppRedis $ Redis.get @Int (mkQueueSkipCountKey r.specialLocationId personId.getId)
      mbGate <- Esq.runInReplica $ QGI.findById (Kernel.Types.Id.Id r.gateId)
      pure (fromMaybe 0 mbSkipCount, mbGate >>= (.maxRideSkipsBeforeQueueRemoval))
    [] -> pure (0, Nothing)
  pure $
    API.Types.UI.SpecialZoneQueue.SpecialZoneQueueRequestListRes
      { requests = allRequests,
        currentSkipCount = skipCount,
        maxSkipsBeforeQueueRemoval = maxSkips
      }
  where
    collectValidActive _ [] = pure []
    collectValidActive now (req : rest) =
      if req.validTill < now
        then do
          QSZQR.updateResponse (Just Domain.Types.SpecialZoneQueueRequest.Ignored) Domain.Types.SpecialZoneQueueRequest.Expired req.id
          collectValidActive now rest
        else do
          rest' <- collectValidActive now rest
          pure (mkRes req : rest')

    collectValidAccepted _ [] = pure []
    collectValidAccepted now (req : rest) = do
      -- Check if arrival timeout has passed for this accepted request
      mbGate <- Esq.runInReplica $ QGI.findById (Kernel.Types.Id.Id req.gateId)
      let arrivalTimeoutSec = maybe 1200 (fromMaybe 1200 . (.pickupZoneArrivalTimeoutInSec)) mbGate
          acceptedAt = req.updatedAt -- updatedAt is set when response changes to Accept
          deadline = addUTCTime (fromIntegral arrivalTimeoutSec) acceptedAt
      if now > deadline
        then do
          -- Past arrival timeout — mark as NoShow in background, don't return to driver
          fork "lazy-noshow-check" $ do
            QSZQR.updateResponse (Just Domain.Types.SpecialZoneQueueRequest.NoShow) Domain.Types.SpecialZoneQueueRequest.Expired req.id
            logInfo $ "Lazy no-show: marked request " <> req.id.getId <> " as NoShow for driver " <> req.driverId.getId
          collectValidAccepted now rest
        else do
          rest' <- collectValidAccepted now rest
          pure (mkRes req : rest')

    mkRes req =
      API.Types.UI.SpecialZoneQueue.SpecialZoneQueueRequestRes
        { requestId = req.id,
          gateId = req.gateId,
          gateName = req.gateName,
          specialLocationId = req.specialLocationId,
          specialLocationName = req.specialLocationName,
          vehicleType = req.vehicleType,
          validTill = req.validTill,
          status = req.status
        }

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
  case actualResponse of
    Domain.Types.SpecialZoneQueueRequest.Accept -> do
      QSZQR.updateResponse (Just Domain.Types.SpecialZoneQueueRequest.Accept) Domain.Types.SpecialZoneQueueRequest.Accepted requestId
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
    _ ->
      QSZQR.updateResponse (Just actualResponse) Domain.Types.SpecialZoneQueueRequest.Expired requestId
  pure Kernel.Types.APISuccess.Success

postSpecialZoneQueueRequestCancel ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postSpecialZoneQueueRequestCancel (mbPersonId, _merchantId, _merchantOpCityId) requestId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person id")
  request <- QSZQR.findByPrimaryKey requestId >>= fromMaybeM (InvalidRequest "Request not found")
  unless (request.driverId == personId) $ throwError (InvalidRequest "Request does not belong to this driver")
  unless (request.status == Domain.Types.SpecialZoneQueueRequest.Accepted) $ throwError (InvalidRequest "Only accepted requests can be cancelled")
  QSZQR.updateResponse (Just Domain.Types.SpecialZoneQueueRequest.Cancelled) Domain.Types.SpecialZoneQueueRequest.Expired requestId
  pure Kernel.Types.APISuccess.Success
