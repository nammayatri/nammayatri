module Product.BecknProvider.Confirm (confirm, calculateDriverPool, getDriverPool) where

import App.Types
import qualified Beckn.Storage.Queries as DB
import qualified Beckn.Storage.Redis.Queries as Redis
import qualified Beckn.Types.Core.API.Confirm as API
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (LatLong))
import qualified Beckn.Types.Mobility.Order as Mobility
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Data.Text as T
import EulerHS.Prelude hiding (id)
import qualified ExternalAPI.Flow as ExternalAPI
import ExternalAPI.Transform as ExternalAPITransform
import qualified Product.BecknProvider.BP as BP
import qualified Storage.Queries.Organization as Organization
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.RideBooking as QRideBooking
import qualified Storage.Queries.RideRequest as RideRequest
import qualified Storage.Queries.SearchReqLocation as QSReqLoc
import qualified Storage.Queries.SearchRequest as SearchRequest
import qualified Storage.Queries.TransporterConfig as QTConf
import Types.App (Driver)
import Types.Error
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.RideBooking as SRB
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
    let transporterId' = quote.providerId
    transporterOrg <-
      Organization.findOrganizationById transporterId'
        >>= fromMaybeM OrgNotFound
    unless (transporterId' == transporterId) $ throwError AccessDenied
    searchRequest <- SearchRequest.findById quote.requestId >>= fromMaybeM SearchRequestNotFound
    let bapOrgId = searchRequest.bapId
    unless (bapOrg.id == Id bapOrgId) $ throwError AccessDenied
    now <- getCurrentTime
    rideBooking <- buildRideBooking searchRequest quote transporterOrg now
    rideRequest <-
      BP.mkRideReq
        (rideBooking.id)
        (transporterOrg.shortId)
        RideRequest.ALLOCATION
        now

    DB.runSqlDBTransaction $ do
      RideRequest.create rideRequest
      QRideBooking.create rideBooking

    bapCallbackUrl <- bapOrg.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
    ExternalAPI.withCallback transporterOrg "confirm" API.onConfirm (req.context) bapCallbackUrl $
      onConfirmCallback
        rideBooking
        searchRequest
        transporterOrg
  where
    buildRideBooking searchRequest quote provider now = do
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
            requestorId = searchRequest.requestorId,
            fromLocationId = searchRequest.fromLocationId,
            toLocationId = searchRequest.toLocationId,
            bapId = searchRequest.bapId,
            price = quote.price,
            distance = quote.distance,
            vehicleVariant = searchRequest.vehicleVariant,
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
  m API.ConfirmOrder
onConfirmCallback rideBooking searchRequest transporterOrg = do
  let transporterId = transporterOrg.id
  let rideBookingId = rideBooking.id
  let pickupPoint = searchRequest.fromLocationId
  let vehicleVariant = searchRequest.vehicleVariant
  driverPool <- map fst <$> calculateDriverPool pickupPoint transporterId vehicleVariant
  setDriverPool rideBookingId driverPool
  logTagInfo "OnConfirmCallback" $ "Driver Pool for Ride " +|| getId rideBookingId ||+ " is set with drivers: " +|| T.intercalate ", " (getId <$> driverPool) ||+ ""
  order <- ExternalAPITransform.mkOrder rideBooking.id Nothing Nothing Mobility.CONFIRMED
  return $ API.ConfirmOrder order

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
      map fst <$> calculateDriverPool pickupPoint orgId vehicleVariant

setDriverPool :: DBFlow m r => Id SRB.RideBooking -> [Id Driver] -> m ()
setDriverPool rideBookingId ids =
  Redis.setExRedis (driverPoolKey rideBookingId) (map getId ids) (60 * 10)

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
