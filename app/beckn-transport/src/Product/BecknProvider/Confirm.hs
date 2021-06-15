module Product.BecknProvider.Confirm (confirm) where

import App.Types
import qualified Beckn.Storage.Queries as DB
import qualified Beckn.Types.Core.API.Confirm as API
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import qualified Beckn.Types.Storage.Vehicle as Vehicle
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified ExternalAPI.Flow as ExternalAPI
import ExternalAPI.Transform as ExternalAPITransform
import qualified Models.Case as Case
import qualified Product.BecknProvider.BP as BP
import Product.Person (calculateDriverPool, setDriverPool)
import qualified Storage.Queries.Case as QCase
import qualified Storage.Queries.Organization as Organization
import qualified Storage.Queries.ProductInstance as QProductInstance
import qualified Storage.Queries.RideRequest as RideRequest
import qualified Test.RandomStrings as RS
import Types.Error
import qualified Types.Storage.RideRequest as RideRequest
import Utils.Common

confirm :: Id Organization.Organization -> Organization.Organization -> API.ConfirmReq -> FlowHandler AckResponse
confirm transporterId bapOrg req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    logTagInfo "confirm API Flow" "Reached"
    BP.validateContext "confirm" $ req.context
    let prodInstId = Id $ req.message.order.id
    productInstance <- QProductInstance.findById' prodInstId >>= fromMaybeM PIDoesNotExist
    let transporterId' = productInstance.organizationId
    unless (productInstance.status == ProductInstance.INSTOCK) $
      throwError $ PIInvalidStatus "This ride cannot be confirmed"
    transporterOrg <- Organization.findOrganizationById transporterId'
    unless (transporterId' == transporterId) $ throwError AccessDenied
    let caseShortId = getId transporterId <> "_" <> req.context.transaction_id
    searchCase <- Case.findBySid caseShortId
    bapOrgId <- searchCase.udf4 & fromMaybeM (CaseFieldNotPresent "udf4")
    unless (bapOrg.id == Id bapOrgId) $ throwError AccessDenied
    orderCase <- mkOrderCase searchCase
    orderProductInstance <- mkOrderProductInstance (orderCase.id) productInstance
    rideRequest <-
      BP.mkRideReq
        (orderProductInstance.id)
        (transporterOrg.shortId)
        RideRequest.ALLOCATION
    let newOrderCaseStatus = Case.INPROGRESS
    let newSearchCaseStatus = Case.COMPLETED
    let newProductInstanceStatus = ProductInstance.CONFIRMED
    Case.validateStatusTransition (orderCase.status) newOrderCaseStatus & fromEitherM CaseInvalidStatus
    Case.validateStatusTransition (searchCase.status) newSearchCaseStatus & fromEitherM CaseInvalidStatus
    ProductInstance.validateStatusTransition (ProductInstance.status productInstance) newProductInstanceStatus
      & fromEitherM PIInvalidStatus
    (currTime, uuid, shortId) <- BP.getIdShortIdAndTime
    let trackerCase = mkTrackerCase searchCase uuid currTime shortId
    trackerProductInstance <- mkTrackerProductInstance (trackerCase.id) productInstance currTime

    DB.runSqlDBTransaction $ do
      QCase.create orderCase
      QProductInstance.create orderProductInstance
      RideRequest.create rideRequest
      QCase.updateStatus (orderCase.id) newOrderCaseStatus
      QCase.updateStatus (searchCase.id) newSearchCaseStatus
      QProductInstance.updateStatus (productInstance.id) newProductInstanceStatus
      QCase.create trackerCase
      QProductInstance.create trackerProductInstance

    bapCallbackUrl <- bapOrg.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
    ExternalAPI.withCallback transporterOrg "confirm" API.onConfirm (req.context) bapCallbackUrl $
      onConfirmCallback
        orderProductInstance
        productInstance
        orderCase
        (trackerCase.id)
        transporterOrg

onConfirmCallback ::
  ProductInstance.ProductInstance ->
  ProductInstance.ProductInstance ->
  Case.Case ->
  Id Case.Case ->
  Organization.Organization ->
  Flow API.ConfirmOrder
onConfirmCallback orderProductInstance productInstance orderCase trackerCaseId transporterOrg = do
  let transporterId = transporterOrg.id
  let prodInstId = productInstance.id
  pickupPoint <- (productInstance.fromLocation) & fromMaybeM (PIFieldNotPresent "location_id")
  vehicleVariant :: Vehicle.Variant <-
    (orderCase.udf1 >>= readMaybe . T.unpack)
      & fromMaybeM (CaseFieldNotPresent "udf1")
  driverPool <- calculateDriverPool pickupPoint transporterId vehicleVariant
  setDriverPool prodInstId driverPool
  logTagInfo "OnConfirmCallback" $ "Driver Pool for Ride " +|| getId prodInstId ||+ " is set with drivers: " +|| T.intercalate ", " (getId <$> driverPool) ||+ ""
  mkOnConfirmPayload orderProductInstance trackerCaseId

mkOrderCase :: Case.Case -> Flow Case.Case
mkOrderCase Case.Case {..} = do
  (now, cid, shortId_) <- BP.getIdShortIdAndTime
  return $
    Case.Case
      { id = cid,
        name = Nothing,
        description = Just "Case to order a Ride",
        shortId = shortId_,
        industry = Case.MOBILITY,
        _type = Case.RIDEORDER,
        parentCaseId = Just id,
        status = Case.INPROGRESS,
        fromLocationId = fromLocationId,
        toLocationId = toLocationId,
        createdAt = now,
        updatedAt = now,
        ..
      }

mkOrderProductInstance :: Id Case.Case -> ProductInstance.ProductInstance -> Flow ProductInstance.ProductInstance
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
        _type = Case.RIDEORDER,
        organizationId = prodInst.organizationId,
        fromLocation = prodInst.fromLocation,
        toLocation = prodInst.toLocation,
        startTime = prodInst.startTime,
        endTime = prodInst.endTime,
        validTill = prodInst.validTill,
        parentId = Just (prodInst.id),
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

mkTrackerCase :: Case.Case -> Text -> UTCTime -> ShortId Case.Case -> Case.Case
mkTrackerCase case_@Case.Case {..} uuid now shortId_ =
  Case.Case
    { id = Id uuid,
      name = Nothing,
      description = Just "Case to track a Ride",
      shortId = shortId_,
      industry = Case.MOBILITY,
      _type = Case.LOCATIONTRACKER,
      status = Case.NEW,
      parentCaseId = Just $ case_.id,
      fromLocationId = case_.fromLocationId,
      toLocationId = case_.toLocationId,
      createdAt = now,
      updatedAt = now,
      ..
    }

mkTrackerProductInstance :: Id Case.Case -> ProductInstance.ProductInstance -> UTCTime -> Flow ProductInstance.ProductInstance
mkTrackerProductInstance caseId prodInst currTime = do
  shortId <- T.pack <$> L.runIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16)
  piId <- L.generateGUID
  return $
    ProductInstance.ProductInstance
      { id = Id piId,
        caseId = caseId,
        productId = prodInst.productId,
        personId = Nothing,
        personUpdatedAt = Nothing,
        shortId = ShortId shortId,
        entityType = ProductInstance.VEHICLE,
        parentId = Just (prodInst.id),
        organizationId = prodInst.organizationId,
        entityId = Nothing,
        _type = Case.LOCATIONTRACKER,
        startTime = prodInst.startTime,
        endTime = prodInst.endTime,
        fromLocation = prodInst.fromLocation,
        toLocation = prodInst.toLocation,
        validTill = prodInst.validTill,
        quantity = 1,
        price = Nothing,
        status = ProductInstance.INSTOCK,
        info = Nothing,
        createdAt = currTime,
        updatedAt = currTime,
        udf1 = prodInst.udf1,
        udf2 = prodInst.udf2,
        udf3 = prodInst.udf3,
        udf4 = prodInst.udf4,
        udf5 = prodInst.udf5
      }

mkOnConfirmPayload :: ProductInstance.ProductInstance -> Id Case.Case -> Flow API.ConfirmOrder
mkOnConfirmPayload orderPI trackerCaseId = do
  trip <- BP.mkTrip trackerCaseId orderPI
  order <- ExternalAPITransform.mkOrder orderPI (Just trip)
  return $ API.ConfirmOrder order
