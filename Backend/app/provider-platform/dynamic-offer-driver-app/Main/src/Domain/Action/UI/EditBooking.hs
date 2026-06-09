module Domain.Action.UI.EditBooking where

import API.Types.UI.EditBooking
import qualified Data.Geohash as DG
import qualified Data.Text as T
import qualified Domain.Action.Internal.ViolationDetection as VID
import qualified Domain.Types.Booking as DBooking
import Domain.Types.BookingUpdateRequest
import qualified Domain.Types.BookingUpdateRequest as DBUR
import qualified Domain.Types.LocationMapping as DLM
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.OnUpdate as OU
import qualified Domain.Types.Person
import qualified Domain.Types.Ride as DRide
import Domain.Types.RideRoute
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps as Maps
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Error.Throwing
import qualified Kernel.Utils.Forkable as Forkable
import Kernel.Utils.Logging (logTagWarning)
import Lib.LocationUpdates.Internal (updatePassedThroughDrop)
import qualified SharedLogic.CallBAP as CallBAP
import qualified SharedLogic.LocationMapping as SLM
import SharedLogic.Ride
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.BookingUpdateRequest as QBUR
import qualified Storage.Queries.BookingUpdateRequestExtra as QBURExtra
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import qualified Storage.Queries.Person as SQP
import qualified Storage.Queries.Ride as QR
import qualified Storage.Queries.SearchRequest as QSR
import qualified Tools.Notifications as Notify

-- Mark all OTHER active BURs (SOFT or USER_CONFIRMED) for this booking as EXPIRED, and notify BAP for each.
-- Called by /update CONFIRM_UPDATE (supersede-on-confirm) and by postEditResultInner ACCEPT (supersede-on-driver-accept).
-- MUST run under L1 (updateLockKey bookingId) so concurrent commits don't race.
supersedeSiblingBURs :: DBooking.Booking -> Kernel.Types.Id.Id DBUR.BookingUpdateRequest -> Environment.Flow ()
supersedeSiblingBURs booking currentBurId = do
  active <- QBURExtra.findAllActiveByBookingId booking.id
  let toExpire = filter (\b -> b.id /= currentBurId) active
  unless (null toExpire) $ do
    QBURExtra.expireActiveSiblings booking.id currentBurId
    void $ Forkable.mapConcurrently (notifyBap . (.bapBookingUpdateRequestId)) toExpire
  where
    notifyBap bapBurId =
      try @_ @SomeException
        (CallBAP.sendUpdateEditDestErrToBAP booking bapBurId "EDIT_SUPERSEDED" "Superseded by a newer edit")
        >>= either
          (\e -> logTagWarning "supersede-notify-bap-failed" (bapBurId <> ": " <> show e))
          (const $ pure ())

snapshotSoftRouteToConfirm :: Text -> Int -> Environment.Flow ()
snapshotSoftRouteToConfirm bId ttl = do
  mbRoute :: Maybe RouteInfo <- Redis.runInMultiCloudRedisMaybeResult (Redis.get (bookingRequestKeySoftUpdate bId))
  whenJust mbRoute $ \r -> Redis.setExp (bookingRequestKeyConfirmUpdate bId) r ttl
  mbMulti :: Maybe [RouteAndDeviationInfo] <- Redis.runInMultiCloudRedisMaybeResult (Redis.safeGet (multipleRouteKeySoftUpdate bId))
  whenJust mbMulti $ \m -> Redis.setExp (multipleRouteKeyConfirmUpdate bId) m ttl

clearConfirmRouteCache :: Text -> Environment.Flow ()
clearConfirmRouteCache bId =
  Redis.runInMultiCloudRedisWrite $ do
    Redis.del (bookingRequestKeyConfirmUpdate bId)
    Redis.del (multipleRouteKeyConfirmUpdate bId)

postEditResult ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.BookingUpdateRequest.BookingUpdateRequest ->
    API.Types.UI.EditBooking.EditBookingRespondAPIReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postEditResult ctx@(_, _, _) bookingUpdateReqId req = do
  bookingUpdateReq <- QBUR.findById bookingUpdateReqId >>= fromMaybeM (InternalError $ "BookingUpdateRequest not found with id:" <> bookingUpdateReqId.getId)
  now <- getCurrentTime
  when (bookingUpdateReq.validTill < now) $ throwError $ InvalidRequest "BookingUpdateRequest is expired"
  when (bookingUpdateReq.status == DBUR.EXPIRED) $ throwError $ InvalidRequest "BookingUpdateRequest was superseded by a newer edit"
  when (bookingUpdateReq.status /= USER_CONFIRMED) $ throwError $ InvalidRequest "BookingUpdateRequest is not in USER_CONFIRMED state"
  let bookingIdText = bookingUpdateReq.bookingId.getId
      l1Key = updateLockKey bookingIdText
  acquired <- Redis.tryLockRedis l1Key 60
  unless acquired $ throwError $ InvalidRequest "Another edit operation is in progress for this booking; please retry"
  ( case bookingUpdateReq.updateType of
      Just DBUR.STOPS -> do
        let n = fromMaybe 0 bookingUpdateReq.preservedPrefixStops
            stopKey = editStopsOrderLockKey bookingIdText (n + 1)
        acquiredStop <- Redis.tryLockRedis stopKey 60
        unless acquiredStop $ throwError $ InvalidRequest "Stop being edited is in flight; please retry"
        let earlyRelease = void (Redis.unlockRedis stopKey) >> void (Redis.unlockRedis l1Key)
        postEditResultInner ctx bookingUpdateReqId req earlyRelease
          `onException` void (Redis.unlockRedis stopKey)
      _ -> do
        let earlyRelease = void (Redis.unlockRedis l1Key)
        postEditResultInner ctx bookingUpdateReqId req earlyRelease
    )
    `onException` void (Redis.unlockRedis l1Key)

postEditResultInner ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.BookingUpdateRequest.BookingUpdateRequest ->
    API.Types.UI.EditBooking.EditBookingRespondAPIReq ->
    Environment.Flow () ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postEditResultInner (mbPersonId, _, _) bookingUpdateReqId EditBookingRespondAPIReq {..} earlyReleaseLocks = do
  driverId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  now <- getCurrentTime
  ( do
      bookingUpdateReq <- QBUR.findById bookingUpdateReqId >>= fromMaybeM (InternalError $ "BookingUpdateRequest not found with id:" <> bookingUpdateReqId.getId)
      when (bookingUpdateReq.status == DBUR.EXPIRED) $ throwError $ InvalidRequest "BookingUpdateRequest was superseded by a newer edit"
      when (bookingUpdateReq.status /= USER_CONFIRMED) $ throwError $ InvalidRequest "BookingUpdateRequest is not in USER_CONFIRMED state"
      when (bookingUpdateReq.validTill < now) $ throwError $ InvalidRequest "BookingUpdateRequest is expired"
      booking <- QB.findById bookingUpdateReq.bookingId >>= fromMaybeM (BookingDoesNotExist bookingUpdateReq.bookingId.getId)
      lockEditDestination <- Redis.tryLockRedis (editDestinationLockKey driverId) 20
      unless lockEditDestination $
        throwError $ InvalidRequest "Driver is processing another ride/edit; please retry"
      if action == ACCEPT
        then do
          let isEditStops = bookingUpdateReq.updateType == Just DBUR.STOPS
          unless isEditStops $ do
            hasAdvancedRide <- QDI.findById driverId <&> maybe False (.hasAdvanceBooking)
            when hasAdvancedRide $ do
              CallBAP.sendUpdateEditDestErrToBAP booking bookingUpdateReq.bapBookingUpdateRequestId "Trip Update Request Not Available" "Driver has an upcoming ride near your drop location. "
              throwError $ InvalidRequest "You have an upcoming ride near your current drop location."
          ride <- QR.findActiveByRBId bookingUpdateReq.bookingId >>= fromMaybeM (InternalError $ "Ride not found for bookingId: " <> bookingUpdateReq.bookingId.getId)
          when (isEditStops && ride.status == DRide.INPROGRESS) $
            VID.ensureNoReachedStopsBeyond ride.id
              (fromMaybe 0 bookingUpdateReq.preservedPrefixStops)
              "Stale state: a stop in the unchanged zone has been reached since the update was requested. Please retry."
          do
            supersedeSiblingBURs booking bookingUpdateReqId
            QR.updateIsPickupOrDestinationEdited (Just True) ride.id
            fork "updateIsReallocationEnabled" $ do
              searchReq <- QSR.findByTransactionIdAndMerchantId booking.transactionId bookingUpdateReq.merchantId >>= fromMaybeM (SearchRequestNotFound $ "transactionId-" <> booking.transactionId <> ",merchantId-" <> bookingUpdateReq.merchantId.getId)
              QSR.updateIsReallocationEnabled (Just False) searchReq.id

            mbDropLocMapRide <- case bookingUpdateReq.updateType of
              Just DBUR.STOPS -> do
                let n = fromMaybe 0 bookingUpdateReq.preservedPrefixStops
                    mbMerchantId = Just bookingUpdateReq.merchantId
                    mbMerchantOpCityId = Just bookingUpdateReq.merchantOperatingCityId
                stopMappings <- QLM.getLatestStopsByEntityId' bookingUpdateReqId.getId
                SLM.rewriteLatestStopMappingsFromOrder bookingUpdateReq.bookingId.getId DLM.BOOKING mbMerchantId mbMerchantOpCityId n stopMappings
                SLM.rewriteLatestStopMappingsFromOrder ride.id.getId DLM.RIDE mbMerchantId mbMerchantOpCityId n stopMappings
                let bookingHasStops = n + length stopMappings > 0
                QB.updateHasStops bookingUpdateReq.bookingId bookingHasStops
                QR.updateHasStops ride.id bookingHasStops
                pure Nothing
              _ -> do
                dropLocMapping <- QLM.getLatestEndByEntityId bookingUpdateReqId.getId >>= fromMaybeM (InternalError $ "Latest drop location mapping not found for bookingUpdateReqId: " <> bookingUpdateReqId.getId)
                prevOrder <- QLM.maxOrderByEntity bookingUpdateReq.bookingId.getId
                dropLocMapBooking <- SLM.buildLocationMapping' dropLocMapping.locationId bookingUpdateReq.bookingId.getId DLM.BOOKING (Just bookingUpdateReq.merchantId) (Just bookingUpdateReq.merchantOperatingCityId) prevOrder
                prevOrderforRide <- QLM.maxOrderByEntity ride.id.getId
                dropLocMapRide <- SLM.buildLocationMapping' dropLocMapping.locationId ride.id.getId DLM.RIDE (Just bookingUpdateReq.merchantId) (Just bookingUpdateReq.merchantOperatingCityId) prevOrderforRide
                QLM.create dropLocMapBooking
                QLM.create dropLocMapRide
                pure (Just dropLocMapRide)
            mbRouteConfirm :: Maybe RouteInfo <- Redis.runInMultiCloudRedisMaybeResult (Redis.get (bookingRequestKeyConfirmUpdate booking.id.getId))
            routeInfo :: RouteInfo <- case mbRouteConfirm of
              Just r -> pure r
              Nothing ->
                Redis.runInMultiCloudRedisMaybeResult (Redis.get (bookingRequestKeySoftUpdate booking.id.getId))
                  >>= fromMaybeM (InternalError $ "BookingRequestRoute not found for bookingId: " <> booking.id.getId)
            mbMultipleConfirm :: Maybe [RouteAndDeviationInfo] <- Redis.runInMultiCloudRedisMaybeResult $ Redis.safeGet $ multipleRouteKeyConfirmUpdate booking.id.getId
            multipleRoutes :: Maybe [RouteAndDeviationInfo] <- case mbMultipleConfirm of
              Just _ -> pure mbMultipleConfirm
              Nothing -> Redis.runInMultiCloudRedisMaybeResult $ Redis.safeGet $ multipleRouteKeySoftUpdate booking.id.getId
            Redis.setExp (searchRequestKey booking.transactionId) routeInfo 3600
            QBUR.updateStatusById DRIVER_ACCEPTED bookingUpdateReqId
            whenJust multipleRoutes $ \allRoutes -> do
              Redis.setExp (multipleRouteKey booking.transactionId) allRoutes 3600
            let estimatedDistance = highPrecMetersToMeters <$> bookingUpdateReq.estimatedDistance
            QB.updateMultipleById bookingUpdateReq.estimatedFare bookingUpdateReq.maxEstimatedDistance estimatedDistance bookingUpdateReq.fareParamsId.getId bookingUpdateReq.bookingId
            recomputeRideFinancialsForFareUpdate booking ride bookingUpdateReq.fareParamsId bookingUpdateReq.estimatedFare

            earlyReleaseLocks -- Release l1 and editStopsOrderLockKey lock keys
            CallBAP.sendUpdateEditDestToBAP booking ride bookingUpdateReq Nothing Nothing OU.CONFIRM_UPDATE
            void $ Redis.unlockRedis (editDestinationLockKey driverId)
            driver <- SQP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
            whenJust mbDropLocMapRide $ \dropLocMapRide -> do
              mbUpdatedloc <- QL.findById dropLocMapRide.locationId
              whenJust mbUpdatedloc $ \updatedloc -> do
                let (lat, lon) = (updatedloc.lat, updatedloc.lon)
                QDI.updateTripEndLocation (Just (Maps.LatLong lat lon)) driverId
                updatePassedThroughDrop driverId
                let mbGeohash = T.pack <$> DG.encode 9 (lat, lon)
                whenJust mbGeohash $ \geohash -> do
                  Redis.setExp (editDestinationUpdatedLocGeohashKey driverId) geohash (2 * 60 * 60)
            clearConfirmRouteCache booking.id.getId
            -- TODO: add notification for edit stops
            case bookingUpdateReq.updateType of
              --   Just DBUR.STOPS -> fork "notifyEditStops" $ Notify.notifyEditStops ride.merchantOperatingCityId driverId driver.deviceToken
              _ -> fork "notifyEditDestination" $ Notify.notifyEditDestination ride.merchantOperatingCityId driverId driver.deviceToken
            return Success
        else do
          -- action /= ACCEPT: driver rejected. Notify BAP and mark BUR rejected.
          CallBAP.sendUpdateEditDestErrToBAP booking bookingUpdateReq.bapBookingUpdateRequestId "Trip Update Request Declined" "Request was declined by your driver. Kindly check with them offline before requesting again."
          QBUR.updateStatusById DRIVER_REJECTED bookingUpdateReq.id
          earlyReleaseLocks -- Release l1 and editStopsOrderLockKey lock keys
          void $ Redis.unlockRedis (editDestinationLockKey driverId)
          clearConfirmRouteCache booking.id.getId

          return Success
    )
