{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.SpecialZoneQueue
  ( getSpecialZoneQueueRequest,
    postSpecialZoneQueueRequestRespond,
    postSpecialZoneQueueRequestCancel,
  )
where

import qualified API.Types.UI.SpecialZoneQueue
import Data.List (partition)
import Data.Time (addUTCTime)
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.SpecialZoneQueueRequest
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common (Seconds (..))
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Queries.GateInfo as QGI
import qualified Lib.Queries.SpecialLocation as LQSL
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator (AllocatorJobType (..), CheckPickupZoneArrivalJobData (..))
import qualified SharedLogic.Allocator.Jobs.SpecialZoneQueue.CheckPickupZoneArrival as ArrivalCheck
import qualified SharedLogic.External.LocationTrackingService.Flow as LTSFlow
import SharedLogic.SpecialZoneDriverDemand (mkQueueSkipCountKey)
import qualified SharedLogic.SpecialZoneDriverDemand as SpecialZoneDriverDemand
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
  -- Fetch both Active and Accepted requests in one query, then split
  allReqs <- QSZQR.findActiveByStasusListAndDriverId personId [Domain.Types.SpecialZoneQueueRequest.Active, Domain.Types.SpecialZoneQueueRequest.Accepted]
  let (activeRequests, acceptedRequests) = partition (\request -> request.status == Domain.Types.SpecialZoneQueueRequest.Active) allReqs
  -- Lazy-expire Active requests past validTill
  validActiveRequests <- collectValidActive now activeRequests
  -- Accepted requests carry an arrivalDeadlineTime (stamped at Accept time).
  -- If it's in the past, fire the (lock-guarded, idempotent) arrival check in
  -- the background and drop the row from the response — matches what the
  -- scheduled CheckPickupZoneArrival job would do, but eagerly so the driver
  -- UI stays in sync without waiting for the job to fire.
  let isStaleAccepted r = case r.arrivalDeadlineTime of
        Just deadline -> deadline < now
        Nothing -> False
      (staleAccepted, freshAccepted) = partition isStaleAccepted acceptedRequests
  forM_ staleAccepted $ \req ->
    fork ("specialZoneArrivalCheck-" <> req.id.getId) $
      ArrivalCheck.runArrivalCheckForRequest
        req.id
        req.driverId
        req.gateId
        req.specialLocationId
        req.vehicleType
        req.merchantId
        req.merchantOperatingCityId
  let acceptedRes = map mkRes freshAccepted
      responseRequests = validActiveRequests ++ acceptedRes
  -- Get skip count from driver's current location's special location
  mbDriverLoc <- LTSFlow.driversLocation [personId]
  mbSpecialLoc <- case mbDriverLoc of
    (loc : _) -> Esq.runInReplica $ LQSL.findSpecialLocationByLatLongFull (LatLong loc.lat loc.lon)
    [] -> pure Nothing
  (skipCount, maxSkips) <- case mbSpecialLoc of
    Just specialLoc -> do
      mbSkipCount <- Redis.withCrossAppRedis $ Redis.get @Int (mkQueueSkipCountKey specialLoc.id.getId personId.getId)
      let mbMaxSkips = Kernel.Prelude.listToMaybe specialLoc.gatesInfo >>= (.maxRideSkipsBeforeQueueRemoval)
      pure (fromMaybe 0 mbSkipCount, mbMaxSkips)
    Nothing -> pure (0, Nothing)
  pure $
    API.Types.UI.SpecialZoneQueue.SpecialZoneQueueRequestListRes
      { requests = responseRequests,
        currentSkipCount = skipCount,
        maxSkipsBeforeQueueRemoval = maxSkips
      }
  where
    collectValidActive _ [] = pure []
    collectValidActive now (req : rest) =
      if req.validTill < now
        then do
          let lockKey = "SpecialZoneQueueRespond:Lock:" <> req.id.getId
          Redis.whenWithLockRedis lockKey 10 $ do
            -- Re-read to ensure the respond handler hasn't already processed this
            mbLatest <- QSZQR.findByPrimaryKey req.id
            whenJust mbLatest $ \latest ->
              when (latest.status == Domain.Types.SpecialZoneQueueRequest.Active) $
                QSZQR.updateResponse (Just Domain.Types.SpecialZoneQueueRequest.Ignored) Domain.Types.SpecialZoneQueueRequest.Expired req.id
          collectValidActive now rest
        else do
          rest' <- collectValidActive now rest
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
  -- Re-read inside a Redis lock to prevent race between the GET handler's
  -- lazy-expire (which sets Ignored/Expired) and this respond call.
  let lockKey = "SpecialZoneQueueRespond:Lock:" <> requestId.getId
  Redis.whenWithLockRedis lockKey 10 $ do
    request <- QSZQR.findByPrimaryKey requestId >>= fromMaybeM (InvalidRequest "Request not found")
    unless (request.driverId == personId) $ throwError (InvalidRequest "Request does not belong to this driver")
    unless (request.status == Domain.Types.SpecialZoneQueueRequest.Active) $ throwError (InvalidRequest "Request is no longer active")
    now <- getCurrentTime
    when (request.validTill < now) $ throwError (InvalidRequest "Request has expired")
    case req.response of
      Domain.Types.SpecialZoneQueueRequest.Accept -> do
        mbGate <- Esq.runInReplica $ QGI.findById (Kernel.Types.Id.Id request.gateId)
        let timeoutSec = maybe 1200 (fromMaybe 1200 . (.pickupZoneArrivalTimeoutInSec)) mbGate
            arrivalDeadline = addUTCTime (fromIntegral timeoutSec) now
        QSZQR.updateToAcceptedWithArrivalDeadline requestId arrivalDeadline
        fork "specialZoneSupplyIncrementOnAccept" $
          SpecialZoneDriverDemand.runSupplyIncrementForRequest requestId.getId request.gateId request.vehicleType
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
        QSZQR.updateResponse (Just req.response) Domain.Types.SpecialZoneQueueRequest.Expired requestId
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
  -- Supply decrement: driver retracted their pickup-zone commitment.
  fork "specialZoneSupplyDecrementOnRequestCancel" $
    SpecialZoneDriverDemand.runSupplyDecrementForRequest requestId.getId request.gateId request.vehicleType
  pure Kernel.Types.APISuccess.Success
