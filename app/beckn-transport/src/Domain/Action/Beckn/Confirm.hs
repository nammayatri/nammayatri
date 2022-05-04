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
import qualified Storage.Queries.Organization as QOrg
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
    phone :: Text,
    mobileCountryCode :: Text
  }

data DOnConfirmReq = DOnConfirmReq
  { rideBookingId :: Id SRB.RideBooking,
    quoteId :: Id DQuote.Quote,
    estimatedTotalFare :: Amount
  }

handler ::
  DOrg.Organization -> DConfirmReq -> Flow DOnConfirmReq
handler transporter req = do
  let quoteId = req.quoteId
      customerMobileCountryCode = req.mobileCountryCode
      customerPhoneNumber = req.phone
  quote <- QQuote.findById quoteId >>= fromMaybeM QuoteDoesNotExist
  let oneWayB = DQuote.isOneWay quote.quoteDetails
  let transporterId' = quote.providerId
  transporterOrg <-
    Organization.findById transporterId'
      >>= fromMaybeM OrgNotFound
  unless (transporterId' == transporter.id) $ throwError AccessDenied
  searchRequest <- SearchRequest.findById quote.requestId >>= fromMaybeM SearchRequestNotFound
  let bapOrgId = searchRequest.bapId
  unless (req.bapId == bapOrgId) $ throwError AccessDenied
  now <- getCurrentTime
  (riderDetails, isNewRider) <- getRiderDetails customerMobileCountryCode customerPhoneNumber now
  rideBooking <- buildRideBooking searchRequest quote transporterOrg riderDetails.id now
  rideRequest <-
    BP.buildRideReq
      (rideBooking.id)
      (transporterOrg.shortId)
      RideRequest.ALLOCATION
      now
  unless oneWayB $ do
    let scheduledTime = addUTCTime (negate $ 60 * 60) rideBooking.startTime
    createScheduleRentalRideRequestJob scheduledTime rideRequest
  Esq.runTransaction $ do
    when isNewRider $ QRD.create riderDetails
    QRideBooking.create rideBooking
    when oneWayB $ RideRequest.create rideRequest
    whenJust quote.discount $ \disc ->
      QDiscTransaction.create $ mkDiscountTransaction rideBooking disc now

  onConfirmCallback rideBooking searchRequest transporterOrg
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
          return $
            SRB.OneWayDetails
              SRB.OneWayRideBookingDetails
                { estimatedDistance = oneWayQuote.distance,
                  ..
                }
        DQuote.RentalDetails -> pure SRB.RentalDetails
      pure SRB.RideBooking {..}

findTransporter :: Id DOrg.Organization -> Flow DOrg.Organization
findTransporter transporterId = do
  transporter <- QOrg.findById transporterId >>= fromMaybeM OrgDoesNotExist
  unless transporter.enabled $ throwError AgencyDisabled
  pure transporter

createScheduleRentalRideRequestJob :: (EsqDBFlow m r) => UTCTime -> RideRequest.RideRequest -> m ()
createScheduleRentalRideRequestJob scheduledAt rideBooking =
  void $ createJobByTime scheduledAt jobEntry
  where
    jobEntry =
      JobEntry
        { jobType = AllocateRental,
          jobData = rideBooking,
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
  Organization.Organization ->
  m DOnConfirmReq
onConfirmCallback rideBooking searchRequest transporterOrg = do
  let transporterId = transporterOrg.id
  let rideBookingId = rideBooking.id
  let pickupPoint = searchRequest.fromLocationId
  let vehicleVariant = rideBooking.vehicleVariant
  driverPool <- recalculateDriverPool pickupPoint rideBookingId transporterId vehicleVariant
  logTagInfo "OnConfirmCallback" $ "Driver Pool for Ride " +|| getId rideBookingId ||+ " is set with drivers: " +|| T.intercalate ", " (getId <$> driverPool) ||+ ""
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
