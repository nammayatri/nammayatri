module Domain.Action.Beckn.Confirm where

import App.Scheduler
import App.Types
import Beckn.External.Encryption (encrypt)
import Beckn.External.GoogleMaps.Types (HasGoogleMaps)
import Beckn.Scheduler
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Amount (Amount)
import Beckn.Types.Id
import qualified Data.Text as T
import Domain.Types.DiscountTransaction
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.Organization as Organization
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.RideBooking as SRB
import qualified Domain.Types.RideRequest as RideRequest
import qualified Domain.Types.RiderDetails as SRD
import qualified Domain.Types.SearchRequest as SearchRequest
import EulerHS.Prelude hiding (id)
import qualified Product.BecknProvider.BP as BP
import SharedLogic.DriverPool (recalculateDriverPool)
import qualified Storage.Queries.DiscountTransaction as QDiscTransaction
import qualified Storage.Queries.Organization as Organization
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.RideBooking as QRideBooking
import qualified Storage.Queries.RideRequest as RideRequest
import qualified Storage.Queries.RiderDetails as QRD
import qualified Storage.Queries.SearchRequest as SearchRequest
import Tools.Metrics
import Types.Error
import Utils.Common

data DConfirmReq = DConfirmReq
  { quoteId :: Id DQuote.Quote,
    bapUri :: BaseUrl,
    bapId :: Text,
    customerPhoneNumber :: Text,
    customerMobileCountryCode :: Text
  }

data DOnConfirmReq = DOnConfirmReq
  { rideBookingId :: Id SRB.RideBooking,
    quoteId :: Id DQuote.Quote,
    estimatedTotalFare :: Amount
  }

handler ::
  DOrg.Organization -> DConfirmReq -> Flow DOnConfirmReq
handler transporter req = do
  quote <- QQuote.findById req.quoteId >>= fromMaybeM QuoteDoesNotExist
  let quoteTransporterId = quote.providerId
  transporterOrg <-
    Organization.findById quoteTransporterId
      >>= fromMaybeM OrgNotFound
  unless (quoteTransporterId == transporter.id) $ throwError AccessDenied
  searchRequest <- SearchRequest.findById quote.requestId >>= fromMaybeM SearchRequestNotFound
  let bapOrgId = searchRequest.bapId
  unless (req.bapId == bapOrgId) $ throwError AccessDenied
  now <- getCurrentTime
  (riderDetails, isNewRider) <- getRiderDetails req.customerMobileCountryCode req.customerPhoneNumber now
  rideBooking <- buildRideBooking searchRequest quote transporterOrg riderDetails.id now
  rideRequest <-
    BP.buildRideReq
      (rideBooking.id)
      (transporterOrg.shortId)
      RideRequest.ALLOCATION
      now
  let transaction additionalDBSaves = Esq.runTransaction $ do
        when isNewRider $ QRD.create riderDetails
        QRideBooking.create rideBooking
        whenJust quote.discount $ \disc ->
          QDiscTransaction.create $ mkDiscountTransaction rideBooking disc now
        additionalDBSaves

      handleRideBookingType (DQuote.OneWayDetails _) =
        transaction $ RideRequest.create rideRequest
      handleRideBookingType DQuote.RentalDetails = do
        transaction $ pure ()
        let secondsPerMinute = 60
        schedulingReserveTime <- secondsToNominalDiffTime <$> asks (.schedulingReserveTime)
        let scheduledTime = addUTCTime (negate schedulingReserveTime) rideBooking.startTime
            schedulingDelay = 0 * secondsPerMinute
            minimalSchedulingTime = addUTCTime (schedulingReserveTime + schedulingDelay) now
        if diffUTCTime scheduledTime now < schedulingDelay
          then
            throwError $
              InvalidRequest $
                "minimum starting time is " <> show minimalSchedulingTime
          else createScheduleRentalRideRequestJob scheduledTime rideRequest

  handleRideBookingType quote.quoteDetails

  onConfirmCallback rideBooking searchRequest transporterOrg.id
  where
    buildRideBooking searchRequest quote provider riderId now = do
      uid <- generateGUID
      let id = Id uid
          transactionId = searchRequest.transactionId
          requestId = searchRequest.id
          quoteId = quote.id
          providerId = provider.id
          startTime = searchRequest.startTime
          fromLocationId = searchRequest.fromLocationId
          bapId = searchRequest.bapId
          bapUri = searchRequest.bapUri
          estimatedFare = quote.estimatedFare
          discount = quote.discount
          estimatedTotalFare = quote.estimatedTotalFare
          vehicleVariant = quote.vehicleVariant
          reallocationsCount = 0
          createdAt = now
          updatedAt = now
          status = SRB.SCHEDULED
      let quoteDetails = quote.quoteDetails
      rideBookingDetails <- case quoteDetails of
        DQuote.OneWayDetails oneWayQuote -> do
          toLocationId <- searchRequest.toLocationId & fromMaybeM (InternalError "ONE_WAY SearchRequest does not have toLocationId")
          pure $
            SRB.OneWayDetails
              SRB.OneWayRideBookingDetails
                { estimatedDistance = oneWayQuote.distance,
                  ..
                }
        DQuote.RentalDetails -> pure SRB.RentalDetails
      pure SRB.RideBooking {..}

createScheduleRentalRideRequestJob :: (EsqDBFlow m r) => UTCTime -> RideRequest.RideRequest -> m ()
createScheduleRentalRideRequestJob scheduledAt rideRequest =
  void $ createJobByTime scheduledAt jobEntry
  where
    --  void $ createJobIn 0 jobEntry -- for debugging purposes

    jobEntry =
      JobEntry
        { jobType = AllocateRental,
          jobData = rideRequest,
          maxErrors = 5
        }

getRiderDetails :: (EncFlow m r, EsqDBFlow m r) => Text -> Text -> UTCTime -> m (SRD.RiderDetails, Bool)
getRiderDetails customerMobileCountryCode customerPhoneNumber now =
  QRD.findByMobileNumber customerPhoneNumber >>= \case
    Nothing -> map (,True) . encrypt =<< buildRiderDetails
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

onConfirmCallback ::
  ( EsqDBFlow m r,
    CoreMetrics m,
    EncFlow m r,
    HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    HasGoogleMaps m r c
  ) =>
  SRB.RideBooking ->
  SearchRequest.SearchRequest ->
  Id Organization.Organization ->
  m DOnConfirmReq
onConfirmCallback rideBooking searchRequest transporterOrgId = do
  let pickupPoint = searchRequest.fromLocationId
  driverPool <- recalculateDriverPool pickupPoint rideBooking.id transporterOrgId rideBooking.vehicleVariant
  logTagInfo "OnConfirmCallback" $ "Driver Pool for Ride " +|| rideBooking.id.getId ||+ " is set with drivers: " +|| T.intercalate ", " (getId <$> driverPool) ||+ ""
  pure $
    DOnConfirmReq
      { rideBookingId = rideBooking.id,
        quoteId = rideBooking.quoteId,
        estimatedTotalFare = rideBooking.estimatedTotalFare
      }

mkDiscountTransaction :: SRB.RideBooking -> Amount -> UTCTime -> DiscountTransaction
mkDiscountTransaction rideBooking discount currTime =
  DiscountTransaction
    { rideBookingId = rideBooking.id,
      organizationId = rideBooking.providerId,
      discount = discount,
      createdAt = currTime
    }
