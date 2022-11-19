module Domain.Action.Beckn.Confirm
  ( confirm,
    DConfirmReq (..),
    DConfirmRes (..),
    ConfirmResBDetails (..),
  )
where

import Beckn.External.Encryption (encrypt)
import Beckn.External.Maps.Google.Config (HasGoogleCfg)
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Hedis (HedisFlow)
import Beckn.Types.Id
import Beckn.Types.Registry (Subscriber (..))
import Beckn.Utils.Common
import qualified Data.Text as T
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Booking.BookingLocation as SBL
import qualified Domain.Types.BusinessEvent as SB
import Domain.Types.DiscountTransaction
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.RideRequest as RideRequest
import qualified Domain.Types.RideRequest as SRideRequest
import qualified Domain.Types.RiderDetails as SRD
import qualified SharedLogic.DriverPool as DrPool
import SharedLogic.Schedule
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.FarePolicy.RentalFarePolicy as QRFP
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Booking.BookingLocation as QBL
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.DiscountTransaction as QDiscTransaction
import qualified Storage.Queries.RideRequest as RideRequest
import qualified Storage.Queries.RiderDetails as QRD
import Tools.Error
import Tools.Metrics

data DConfirmReq = DConfirmReq
  { bookingId :: Id SRB.Booking,
    customerMobileCountryCode :: Text,
    customerPhoneNumber :: Text,
    fromAddress :: SBL.LocationAddress,
    toAddress :: Maybe SBL.LocationAddress,
    mbRiderName :: Maybe Text
  }

data DConfirmRes = DConfirmRes
  { booking :: SRB.Booking,
    bDetails :: ConfirmResBDetails,
    fromLocation :: SBL.BookingLocation,
    toLocation :: Maybe SBL.BookingLocation,
    riderDetails :: SRD.RiderDetails,
    transporter :: DM.Merchant
  }

data ConfirmResBDetails
  = OneWayDetails
  | RentalDetails
      { baseDuration :: Hours,
        baseDistance :: Kilometers
      }

confirm ::
  ( HasCacheConfig r,
    EncFlow m r,
    EsqDBFlow m r,
    HedisFlow m r,
    HasFlowEnv m r ["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    HasFlowEnv m r '["schedulingReserveTime" ::: Seconds],
    CoreMetrics m,
    HasGoogleCfg r
  ) =>
  Id DM.Merchant ->
  Subscriber ->
  DConfirmReq ->
  m DConfirmRes
confirm transporterId subscriber req = do
  booking <- QRB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  let transporterId' = booking.providerId
  transporter <-
    CQM.findById transporterId'
      >>= fromMaybeM (MerchantNotFound transporterId'.getId)
  unless (transporterId' == transporterId) $ throwError AccessDenied
  let bapMerchantId = booking.bapId
  unless (subscriber.subscriber_id == bapMerchantId) $ throwError AccessDenied
  now <- getCurrentTime
  (riderDetails, isNewRider) <- getRiderDetails req.customerMobileCountryCode req.customerPhoneNumber now
  rideRequest <-
    buildRideReq
      booking.id
      transporter.subscriberId
      now

  let finalTransaction addons = Esq.runTransaction $ do
        when isNewRider $ QRD.create riderDetails
        QRB.updateStatus booking.id SRB.CONFIRMED
        QRB.updateRiderId booking.id riderDetails.id
        whenJust req.mbRiderName $ QRB.updateRiderName booking.id
        QBL.updateAddress booking.fromLocation.id req.fromAddress
        whenJust booking.discount $ \disc ->
          QDiscTransaction.create $ mkDiscountTransaction booking disc now
        addons

  let fromLocation = booking.fromLocation
  res <- case booking.bookingDetails of
    SRB.OneWayDetails details -> do
      finalTransaction $ do
        RideRequest.create rideRequest
        whenJust req.toAddress $ \toAddr -> QBL.updateAddress details.toLocation.id toAddr
      return $
        DConfirmRes
          { toLocation = Just details.toLocation,
            bDetails = OneWayDetails,
            ..
          }
    SRB.RentalDetails details -> do
      let secondsPerMinute = 60
      schedulingReserveTime <- secondsToNominalDiffTime <$> asks (.schedulingReserveTime)
      let scheduledTime = addUTCTime (negate schedulingReserveTime) booking.startTime
          presentIntervalWidth = 5 * secondsPerMinute -- 5 minutes
      case compareTimeWithInterval presentIntervalWidth scheduledTime now of
        LT -> throwError $ InvalidRequest "impossible to book a ride for the past"
        EQ -> finalTransaction $ RideRequest.create rideRequest
        GT ->
          finalTransaction $
            createScheduleRentalRideRequestJob scheduledTime $
              AllocateRentalJobData
                { bookingId = booking.id,
                  shortOrgId = transporter.subscriberId
                }
      rentalFP <- QRFP.findById details.rentalFarePolicyId >>= fromMaybeM NoFarePolicy
      return $
        DConfirmRes
          { toLocation = Nothing,
            bDetails = RentalDetails {baseDistance = rentalFP.baseDistance, baseDuration = rentalFP.baseDuration},
            ..
          }

  driverPoolResults <- DrPool.recalculateDriverPool booking
  Esq.runTransaction $ traverse_ (QBE.logDriverInPoolEvent SB.ON_CONFIRM (Just booking.id)) driverPoolResults
  let driverPool = map (.driverId) driverPoolResults
  logTagInfo "OnConfirmCallback" $
    "Driver Pool for Ride " <> booking.id.getId <> " is set with drivers: "
      <> T.intercalate ", " (getId <$> driverPool)
  Esq.runTransaction $ QBE.logRideConfirmedEvent booking.id
  return res
  where
    buildRideReq bookingId subscriberId now = do
      guid <- generateGUID
      pure
        SRideRequest.RideRequest
          { id = Id guid,
            bookingId = bookingId,
            subscriberId = subscriberId,
            createdAt = now,
            _type = RideRequest.ALLOCATION,
            info = Nothing
          }

getRiderDetails :: (EncFlow m r, EsqDBFlow m r) => Text -> Text -> UTCTime -> m (SRD.RiderDetails, Bool)
getRiderDetails customerMobileCountryCode customerPhoneNumber now =
  QRD.findByMobileNumber customerPhoneNumber >>= \case
    Nothing -> fmap (,True) . encrypt =<< buildRiderDetails
    Just a -> return (a, False)
  where
    buildRiderDetails = do
      id <- generateGUID
      return $
        SRD.RiderDetails
          { id = id,
            mobileCountryCode = customerMobileCountryCode,
            mobileNumber = customerPhoneNumber,
            createdAt = now,
            updatedAt = now
          }

mkDiscountTransaction :: SRB.Booking -> Money -> UTCTime -> DiscountTransaction
mkDiscountTransaction booking discount currTime =
  DiscountTransaction
    { bookingId = booking.id,
      merchantId = booking.providerId,
      discount = discount,
      createdAt = currTime
    }
