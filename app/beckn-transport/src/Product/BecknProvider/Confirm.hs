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
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.Case as QCase
import qualified Storage.Queries.Organization as Organization
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.ProductInstance as QProductInstance
import qualified Storage.Queries.RideRequest as RideRequest
import qualified Storage.Queries.SearchReqLocation as QSReqLoc
import qualified Storage.Queries.TransporterConfig as QTConf
import Types.App (Driver)
import Types.Error
import qualified Types.Storage.Case as Case
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.ProductInstance as ProductInstance
import qualified Types.Storage.RideRequest as RideRequest
import qualified Types.Storage.SearchReqLocation as SSReqLoc
import qualified Types.Storage.TransporterConfig as STConf
import qualified Types.Storage.Vehicle as SV
import qualified Types.Storage.Vehicle as Vehicle
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
    let prodInstId = Id $ req.message.order.id
    productInstance <- QProductInstance.findById' prodInstId >>= fromMaybeM PIDoesNotExist
    let transporterId' = productInstance.organizationId
    unless (productInstance.status == ProductInstance.INSTOCK) $
      throwError $ PIInvalidStatus "This ride cannot be confirmed"
    transporterOrg <-
      Organization.findOrganizationById transporterId'
        >>= fromMaybeM OrgNotFound
    unless (transporterId' == transporterId) $ throwError AccessDenied
    let caseShortId = getId transporterId <> "_" <> req.context.transaction_id
    searchCase <- Case.findBySid caseShortId >>= fromMaybeM CaseNotFound
    bapOrgId <- searchCase.udf4 & fromMaybeM (CaseFieldNotPresent "udf4")
    unless (bapOrg.id == Id bapOrgId) $ throwError AccessDenied
    orderProductInstance <- mkOrderProductInstance (searchCase.id) productInstance
    rideRequest <-
      BP.mkRideReq
        (orderProductInstance.id)
        (transporterOrg.shortId)
        RideRequest.ALLOCATION
    let newSearchCaseStatus = Case.COMPLETED
    let newProductInstanceStatus = ProductInstance.CONFIRMED
    Case.validateStatusTransition (searchCase.status) newSearchCaseStatus & fromEitherM CaseInvalidStatus
    ProductInstance.validateStatusTransition (ProductInstance.status productInstance) newProductInstanceStatus
      & fromEitherM PIInvalidStatus

    DB.runSqlDBTransaction $ do
      QProductInstance.create orderProductInstance
      RideRequest.create rideRequest
      QCase.updateStatus (searchCase.id) newSearchCaseStatus
      QProductInstance.updateStatus (productInstance.id) newProductInstanceStatus

    bapCallbackUrl <- bapOrg.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
    ExternalAPI.withCallback transporterOrg "confirm" API.onConfirm (req.context) bapCallbackUrl $
      onConfirmCallback
        orderProductInstance
        productInstance
        searchCase
        transporterOrg

onConfirmCallback ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds]
  ) =>
  ProductInstance.ProductInstance ->
  ProductInstance.ProductInstance ->
  Case.Case ->
  Organization.Organization ->
  m API.ConfirmOrder
onConfirmCallback orderProductInstance productInstance searchCase transporterOrg = do
  let transporterId = transporterOrg.id
  let prodInstId = productInstance.id
  pickupPoint <- (productInstance.fromLocation) & fromMaybeM (PIFieldNotPresent "location_id")
  vehicleVariant :: Vehicle.Variant <-
    (searchCase.udf1 >>= readMaybe . T.unpack)
      & fromMaybeM (CaseFieldNotPresent "udf1")
  driverPool <- map fst <$> calculateDriverPool pickupPoint transporterId vehicleVariant
  setDriverPool prodInstId driverPool
  logTagInfo "OnConfirmCallback" $ "Driver Pool for Ride " +|| getId prodInstId ||+ " is set with drivers: " +|| T.intercalate ", " (getId <$> driverPool) ||+ ""
  mkOnConfirmPayload orderProductInstance

driverPoolKey :: Id ProductInstance.ProductInstance -> Text
driverPoolKey = ("beckn:driverpool:" <>) . getId

getDriverPool ::
  ( DBFlow m r,
    HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds]
  ) =>
  Id ProductInstance.ProductInstance ->
  m [Id Driver]
getDriverPool piId =
  Redis.getKeyRedis (driverPoolKey piId)
    >>= maybe calcDriverPool (pure . map Id)
  where
    calcDriverPool = do
      prodInst <- QProductInstance.findById piId >>= fromMaybeM PIDoesNotExist
      case_ <- Case.findById (prodInst.caseId) >>= fromMaybeM CaseNotFound
      vehicleVariant :: SV.Variant <-
        (case_.udf1 >>= readMaybe . T.unpack)
          & fromMaybeM (CaseFieldNotPresent "udf1")
      pickupPoint <-
        prodInst.fromLocation
          & fromMaybeM (PIFieldNotPresent "location_id")
      let orgId = prodInst.organizationId
      map fst <$> calculateDriverPool pickupPoint orgId vehicleVariant

setDriverPool :: DBFlow m r => Id ProductInstance.ProductInstance -> [Id Driver] -> m ()
setDriverPool piId ids =
  Redis.setExRedis (driverPoolKey piId) (map getId ids) (60 * 10)

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

mkOrderProductInstance :: MonadFlow m => Id Case.Case -> ProductInstance.ProductInstance -> m ProductInstance.ProductInstance
mkOrderProductInstance caseId prodInst = do
  (now, pid, shortId) <- BP.getIdShortIdAndTime
  inAppOtpCode <- generateOTPCode
  return $
    ProductInstance.ProductInstance
      { id = Id pid,
        caseId = caseId,
        productId = prodInst.productId,
        personId = Nothing,
        personUpdatedAt = Nothing,
        entityType = ProductInstance.VEHICLE,
        entityId = Nothing,
        shortId = shortId,
        quantity = 1,
        price = prodInst.price,
        actualPrice = Nothing,
        _type = ProductInstance.RIDEORDER,
        organizationId = prodInst.organizationId,
        fromLocation = prodInst.fromLocation,
        toLocation = prodInst.toLocation,
        startTime = prodInst.startTime,
        endTime = prodInst.endTime,
        validTill = prodInst.validTill,
        parentId = Just (prodInst.id),
        distance = 0,
        status = ProductInstance.CONFIRMED,
        info = Nothing,
        createdAt = now,
        updatedAt = now,
        udf1 = prodInst.udf1,
        udf2 = prodInst.udf2,
        udf3 = prodInst.udf3,
        udf4 = Just inAppOtpCode,
        udf5 = prodInst.udf5
      }

mkOnConfirmPayload :: (DBFlow m r, EncFlow m r) => ProductInstance.ProductInstance -> m API.ConfirmOrder
mkOnConfirmPayload orderPI = do
  trip <- BP.mkTrip orderPI
  searchPiId <- orderPI.parentId & fromMaybeM (PIFieldNotPresent "parentId")
  order <- ExternalAPITransform.mkOrder searchPiId (Just trip) Nothing
  return $ API.ConfirmOrder order
