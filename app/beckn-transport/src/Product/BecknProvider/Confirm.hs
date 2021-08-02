module Product.BecknProvider.Confirm (confirm, calculateDriverPool, getDriverPool) where

import App.Types
import qualified Beckn.Storage.Queries as DB
import qualified Beckn.Storage.Redis.Queries as Redis
import qualified Beckn.Types.Core.API.Confirm as API
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (LatLong))
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Data.Text as T
import EulerHS.Prelude hiding (id)
import qualified ExternalAPI.Flow as ExternalAPI
import ExternalAPI.Transform as ExternalAPITransform
import qualified Product.BecknProvider.BP as BP
import qualified Storage.Queries.Organization as Organization
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideRequest as RideRequest
import qualified Storage.Queries.SearchReqLocation as QSReqLoc
import qualified Storage.Queries.SearchRequest as SearchRequest
import qualified Storage.Queries.TransporterConfig as QTConf
import Types.App (Driver)
import Types.Error
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.Quote as Quote
import qualified Types.Storage.Ride as Ride
import qualified Types.Storage.RideRequest as RideRequest
import qualified Types.Storage.SearchReqLocation as SSReqLoc
import qualified Types.Storage.SearchRequest as SearchRequest
import qualified Types.Storage.TransporterConfig as STConf
import qualified Types.Storage.Vehicle as SV
import Utils.Common

confirm ::
  Id Organization.Organization ->
  SignatureAuthResult Organization.Organization ->
  API.ConfirmReq ->
  FlowHandler AckResponse
confirm transporterId (SignatureAuthResult _ bapOrg) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    logTagInfo "confirm API Flow" "Reached"
    BP.validateContext "confirm" $ req.context
    let quoteId = Id $ req.message.order.id
    quote <- QQuote.findById' quoteId >>= fromMaybeM QuoteDoesNotExist
    let transporterId' = quote.organizationId
    unless (quote.status == Quote.INSTOCK) $
      throwError $ QuoteInvalidStatus "This ride cannot be confirmed"
    transporterOrg <-
      Organization.findOrganizationById transporterId'
        >>= fromMaybeM OrgNotFound
    unless (transporterId' == transporterId) $ throwError AccessDenied
    searchRequest <- SearchRequest.findByTxnIdAndProviderId req.context.transaction_id transporterId >>= fromMaybeM SearchRequestNotFound
    let bapOrgId = searchRequest.bapId
    unless (bapOrg.id == Id bapOrgId) $ throwError AccessDenied
    ride <- mkRide quote
    rideRequest <-
      BP.mkRideReq
        (ride.id)
        (transporterOrg.shortId)
        RideRequest.ALLOCATION
    let newQuoteStatus = Quote.CONFIRMED
    Quote.validateStatusTransition (Quote.status quote) newQuoteStatus
      & fromEitherM QuoteInvalidStatus

    DB.runSqlDBTransaction $ do
      QRide.create ride
      RideRequest.create rideRequest
      QQuote.updateStatus (quote.id) newQuoteStatus

    bapCallbackUrl <- bapOrg.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
    ExternalAPI.withCallback transporterOrg "confirm" API.onConfirm (req.context) bapCallbackUrl $
      onConfirmCallback
        ride
        quote
        searchRequest
        transporterOrg

onConfirmCallback ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds]
  ) =>
  Ride.Ride ->
  Quote.Quote ->
  SearchRequest.SearchRequest ->
  Organization.Organization ->
  m API.ConfirmOrder
onConfirmCallback ride quote searchRequest transporterOrg = do
  let transporterId = transporterOrg.id
  let quoteId = ride.id
  pickupPoint <- (quote.fromLocation) & fromMaybeM (QuoteFieldNotPresent "location_id")
  let vehicleVariant = searchRequest.vehicleVariant
  driverPool <- map fst <$> calculateDriverPool pickupPoint transporterId vehicleVariant
  setDriverPool quoteId driverPool
  logTagInfo "OnConfirmCallback" $ "Driver Pool for Ride " +|| getId quoteId ||+ " is set with drivers: " +|| T.intercalate ", " (getId <$> driverPool) ||+ ""
  mkOnConfirmPayload ride

driverPoolKey :: Id Ride.Ride -> Text
driverPoolKey = ("beckn:driverpool:" <>) . getId

getDriverPool ::
  ( DBFlow m r,
    HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds]
  ) =>
  Id Ride.Ride ->
  m [Id Driver]
getDriverPool rideId =
  Redis.getKeyRedis (driverPoolKey rideId)
    >>= maybe calcDriverPool (pure . map Id)
  where
    calcDriverPool = do
      ride <- QRide.findById rideId >>= fromMaybeM RideDoesNotExist
      searchRequest <- SearchRequest.findById (ride.requestId) >>= fromMaybeM SearchRequestNotFound
      let vehicleVariant = searchRequest.vehicleVariant
      pickupPoint <-
        ride.fromLocation
          & fromMaybeM (RideFieldNotPresent "location_id")
      let orgId = ride.organizationId
      map fst <$> calculateDriverPool pickupPoint orgId vehicleVariant

setDriverPool :: DBFlow m r => Id Ride.Ride -> [Id Driver] -> m ()
setDriverPool rideId ids =
  Redis.setExRedis (driverPoolKey rideId) (map getId ids) (60 * 10)

calculateDriverPool ::
  ( DBFlow m r,
    HasFlowEnv m r ["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds]
  ) =>
  Id SSReqLoc.SearchReqLocation ->
  Id Organization.Organization ->
  SV.Variant ->
  m [(Id Driver, Double)]
calculateDriverPool locId orgId variant = do
  location <- QSReqLoc.findLocationById locId >>= fromMaybeM LocationNotFound
  let lat = location.lat
      long = location.long
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

mkRide :: MonadFlow m => Quote.Quote -> m Ride.Ride
mkRide quote = do
  (now, pid, shortId) <- BP.getIdShortIdAndTime
  inAppOtpCode <- generateOTPCode
  return $
    Ride.Ride
      { id = Id pid,
        requestId = quote.requestId,
        productId = quote.productId,
        personId = Nothing,
        personUpdatedAt = Nothing,
        entityType = Ride.VEHICLE,
        entityId = Nothing,
        shortId = shortId,
        quantity = 1,
        price = quote.price,
        actualPrice = Nothing,
        organizationId = quote.organizationId,
        fromLocation = quote.fromLocation,
        toLocation = quote.toLocation,
        startTime = quote.startTime,
        endTime = quote.endTime,
        validTill = quote.validTill,
        quoteId = quote.id,
        distance = 0,
        status = Ride.CONFIRMED,
        info = Nothing,
        createdAt = now,
        updatedAt = now,
        udf1 = quote.udf1,
        udf2 = quote.udf2,
        udf3 = quote.udf3,
        udf4 = Just inAppOtpCode,
        udf5 = quote.udf5
      }

mkOnConfirmPayload :: (DBFlow m r, EncFlow m r) => Ride.Ride -> m API.ConfirmOrder
mkOnConfirmPayload ride = do
  trip <- BP.mkTrip ride
  let quoteId = ride.quoteId
  order <- ExternalAPITransform.mkOrder quoteId (Just trip) Nothing
  return $ API.ConfirmOrder order
