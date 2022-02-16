module Product.BecknProvider.Confirm (confirm, calculateDriverPool, getDriverPool) where

import App.Types
import Beckn.External.Encryption (encrypt)
import Beckn.Product.Validation.Context
import qualified Beckn.Storage.Queries as DB
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Amount (Amount)
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Taxi.API.Confirm as Confirm
import qualified Beckn.Types.Core.Taxi.API.OnConfirm as OnConfirm
import Beckn.Types.Core.Taxi.Common.Context (Action (CONFIRM))
import qualified Beckn.Types.Core.Taxi.OnConfirm as OnConfirm
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (LatLong))
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Data.Text as T
import EulerHS.Prelude hiding (id)
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Product.BecknProvider.BP as BP
import qualified Storage.Queries.DiscountTransaction as QDiscTransaction
import qualified Storage.Queries.Organization as Organization
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.RideBooking as QRideBooking
import qualified Storage.Queries.RideRequest as RideRequest
import qualified Storage.Queries.RiderDetails as QRD
import qualified Storage.Queries.SearchReqLocation as QSReqLoc
import qualified Storage.Queries.SearchRequest as SearchRequest
import qualified Storage.Queries.TransporterConfig as QTConf
import Types.App (Driver)
import Types.Error
import Types.Storage.DiscountTransaction
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.RideBooking as SRB
import qualified Types.Storage.RideRequest as RideRequest
import qualified Types.Storage.RiderDetails as SRD
import qualified Types.Storage.SearchReqLocation as SSReqLoc
import qualified Types.Storage.SearchRequest as SearchRequest
import qualified Types.Storage.TransporterConfig as STConf
import qualified Types.Storage.Vehicle as SV
import Utils.Common

confirm ::
  Id Organization.Organization ->
  SignatureAuthResult ->
  Confirm.ConfirmReq ->
  FlowHandler AckResponse
confirm transporterId (SignatureAuthResult _ subscriber) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    logTagInfo "confirm API Flow" "Reached"
    validateContext req.context
    let items = req.message.order.items
    item <- case items of
      [] -> throwError (InvalidRequest "List of confirmed items is empty.")
      [item] -> return item
      _ -> throwError (InvalidRequest "List of confirmed items must contain exactly one item.")
    let quoteId = Id item.id
        phone = req.message.order.fulfillment.customer.contact.phone
        customerMobileCountryCode = phone.country_code
        customerPhoneNumber = phone.number
    quote <- QQuote.findById' quoteId >>= fromMaybeM QuoteDoesNotExist
    let transporterId' = quote.providerId
    transporterOrg <-
      Organization.findOrganizationById transporterId'
        >>= fromMaybeM OrgNotFound
    unless (transporterId' == transporterId) $ throwError AccessDenied
    searchRequest <- SearchRequest.findById quote.requestId >>= fromMaybeM SearchRequestNotFound
    let bapOrgId = searchRequest.bapId
    unless (subscriber.subscriber_id == bapOrgId) $ throwError AccessDenied
    now <- getCurrentTime
    (riderDetails, isNewRider) <- getRiderDetails customerMobileCountryCode customerPhoneNumber now
    rideBooking <- buildRideBooking searchRequest quote transporterOrg riderDetails.id now
    rideRequest <-
      BP.buildRideReq
        (rideBooking.id)
        (transporterOrg.shortId)
        RideRequest.ALLOCATION
        now

    DB.runSqlDBTransaction $ do
      when isNewRider $ QRD.create riderDetails
      QRideBooking.create rideBooking
      RideRequest.create rideRequest
      whenJust quote.discount $ \disc ->
        QDiscTransaction.create $ mkDiscountTransaction rideBooking disc now

    let bapCallbackUrl = req.context.bap_uri
    ExternalAPI.withCallback transporterOrg CONFIRM OnConfirm.onConfirmAPI (req.context) bapCallbackUrl $
      onConfirmCallback
        rideBooking
        searchRequest
        transporterOrg
  where
    buildRideBooking searchRequest quote provider riderId now = do
      id <- generateGUID
      return $
        SRB.RideBooking
          { id = Id id,
            transactionId = searchRequest.transactionId,
            requestId = searchRequest.id,
            quoteId = quote.id,
            status = SRB.CONFIRMED,
            providerId = provider.id,
            startTime = searchRequest.startTime,
            riderId = riderId,
            fromLocationId = searchRequest.fromLocationId,
            toLocationId = searchRequest.toLocationId,
            bapId = searchRequest.bapId,
            bapUri = searchRequest.bapUri,
            estimatedFare = quote.estimatedFare,
            discount = quote.discount,
            estimatedTotalFare = quote.estimatedTotalFare,
            distance = quote.distance,
            vehicleVariant = quote.vehicleVariant,
            reallocationsCount = 0,
            createdAt = now,
            updatedAt = now
          }

getRiderDetails :: (EncFlow m r, DBFlow m r) => Text -> Text -> UTCTime -> m (SRD.RiderDetails, Bool)
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
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds]
  ) =>
  SRB.RideBooking ->
  SearchRequest.SearchRequest ->
  Organization.Organization ->
  m OnConfirm.OnConfirmMessage
onConfirmCallback rideBooking searchRequest transporterOrg = do
  let transporterId = transporterOrg.id
  let rideBookingId = rideBooking.id
  let pickupPoint = searchRequest.fromLocationId
  let vehicleVariant = rideBooking.vehicleVariant
  driverPool <- map (.driverId) <$> calculateDriverPool pickupPoint transporterId (Just vehicleVariant)
  setDriverPool rideBookingId driverPool
  logTagInfo "OnConfirmCallback" $ "Driver Pool for Ride " +|| getId rideBookingId ||+ " is set with drivers: " +|| T.intercalate ", " (getId <$> driverPool) ||+ ""
  return $ OnConfirm.OnConfirmMessage order
  where
    order =
      OnConfirm.Order
        { id = rideBooking.id.getId,
          items = [OnConfirm.OrderItem $ rideBooking.quoteId.getId],
          estimated_total_fare = OnConfirm.Price $ realToFrac rideBooking.estimatedTotalFare
        }

driverPoolKey :: Id SRB.RideBooking -> Text
driverPoolKey = ("beckn:driverpool:" <>) . getId

getDriverPool ::
  ( DBFlow m r,
    HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds]
  ) =>
  Id SRB.RideBooking ->
  m [Id Driver]
getDriverPool rideBookingId =
  Redis.getKeyRedis (driverPoolKey rideBookingId)
    >>= maybe calcDriverPool (pure . map Id)
  where
    calcDriverPool = do
      rideBooking <- QRideBooking.findById rideBookingId >>= fromMaybeM RideBookingDoesNotExist
      let vehicleVariant = rideBooking.vehicleVariant
          pickupPoint = rideBooking.fromLocationId
          orgId = rideBooking.providerId
      map (.driverId) <$> calculateDriverPool pickupPoint orgId (Just vehicleVariant)

setDriverPool :: DBFlow m r => Id SRB.RideBooking -> [Id Driver] -> m ()
setDriverPool rideBookingId ids =
  Redis.setExRedis (driverPoolKey rideBookingId) (map getId ids) (60 * 10)

-- TODO: move this function somewhere to avoid Product.Search importing Product.Confirm
calculateDriverPool ::
  ( DBFlow m r,
    HasFlowEnv m r ["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds]
  ) =>
  Id SSReqLoc.SearchReqLocation ->
  Id Organization.Organization ->
  Maybe SV.Variant ->
  m [QP.DriverPoolResult]
calculateDriverPool locId orgId variant = do
  location <- QSReqLoc.findLocationById locId >>= fromMaybeM LocationNotFound
  let lat = location.lat
      long = location.lon
  radius <- getRadius
  measuringDurationToLog INFO "calculateDriverPool" $
    QP.getNearestDrivers
      (LatLong lat long)
      radius
      orgId
      variant
  where
    getRadius =
      QTConf.findValueByOrgIdAndKey orgId (STConf.ConfigKey "radius")
        >>= maybe
          (fromIntegral <$> asks (.defaultRadiusOfSearch))
          radiusFromTransporterConfig
    radiusFromTransporterConfig conf =
      fromMaybeM (InternalError "The radius is not a number.")
        . readMaybe
        . toString
        $ conf.value

mkDiscountTransaction :: SRB.RideBooking -> Amount -> UTCTime -> DiscountTransaction
mkDiscountTransaction rideBooking discount currTime =
  DiscountTransaction
    { rideBookingid = rideBooking.id,
      organizationId = rideBooking.providerId,
      discount = discount,
      createdAt = currTime
    }
