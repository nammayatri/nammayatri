module Product.BecknProvider.Confirm (confirm) where

import App.Types
import qualified Beckn.Storage.Queries as DB
import qualified Beckn.Types.Core.API.Confirm as API
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified ExternalAPI.Flow as ExternalAPI
import ExternalAPI.Transform as ExternalAPITransform
import qualified Product.BecknProvider.BP as BP
import Product.Person (calculateDriverPool, setDriverPool)
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.Case as QCase
import qualified Storage.Queries.Organization as Organization
import qualified Storage.Queries.ProductInstance as QProductInstance
import qualified Storage.Queries.RideRequest as RideRequest
import qualified Test.RandomStrings as RS
import Types.Error
import qualified Types.Storage.Case as Case
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.ProductInstance as ProductInstance
import qualified Types.Storage.RideRequest as RideRequest
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
        transporterOrg

onConfirmCallback ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds]
  ) =>
  ProductInstance.ProductInstance ->
  Organization.Organization ->
  m API.ConfirmOrder
onConfirmCallback orderPi transporterOrg = do
  let transporterId = transporterOrg.id
  searchPiId <- orderPi.parentId & fromMaybeM (PIFieldNotPresent "parent_id")
  pickupPoint <- (orderPi.fromLocation) & fromMaybeM (PIFieldNotPresent "location_id")
  driverPool <- map (.driverId) <$> calculateDriverPool pickupPoint transporterId (Just orderPi.vehicleVariant)
  setDriverPool searchPiId driverPool
  logTagInfo "OnConfirmCallback" $ "Driver Pool for Ride " +|| getId searchPiId ||+ " is set with drivers: " +|| T.intercalate ", " (getId <$> driverPool) ||+ ""
  mkOnConfirmPayload orderPi

mkOrderCase :: MonadFlow m => Case.Case -> m Case.Case
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

mkOrderProductInstance :: MonadFlow m => Id Case.Case -> ProductInstance.ProductInstance -> m ProductInstance.ProductInstance
mkOrderProductInstance caseId' ProductInstance.ProductInstance {..} = do
  (now, pid, shortId') <- BP.getIdShortIdAndTime
  inAppOtpCode <- generateOTPCode
  return $
    ProductInstance.ProductInstance
      { id = Id pid,
        caseId = caseId',
        personId = Nothing,
        personUpdatedAt = Nothing,
        entityType = ProductInstance.VEHICLE,
        entityId = Nothing,
        shortId = shortId',
        quantity = 1,
        actualPrice = Nothing,
        _type = Case.RIDEORDER,
        traveledDistance = 0,
        chargeableDistance = Nothing,
        parentId = Just id,
        status = ProductInstance.CONFIRMED,
        info = Nothing,
        createdAt = now,
        updatedAt = now,
        udf4 = Just inAppOtpCode,
        ..
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

mkTrackerProductInstance :: MonadFlow m => Id Case.Case -> ProductInstance.ProductInstance -> UTCTime -> m ProductInstance.ProductInstance
mkTrackerProductInstance caseId' ProductInstance.ProductInstance {..} currTime = do
  shortId' <- T.pack <$> L.runIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16)
  piId <- L.generateGUID
  return $
    ProductInstance.ProductInstance
      { id = Id piId,
        caseId = caseId',
        personId = Nothing,
        personUpdatedAt = Nothing,
        shortId = ShortId shortId',
        entityType = ProductInstance.VEHICLE,
        parentId = Just id,
        entityId = Nothing,
        traveledDistance = 0,
        chargeableDistance = Nothing,
        _type = Case.LOCATIONTRACKER,
        quantity = 1,
        actualPrice = Nothing,
        status = ProductInstance.INSTOCK,
        info = Nothing,
        createdAt = currTime,
        updatedAt = currTime,
        ..
      }

mkOnConfirmPayload :: (DBFlow m r, EncFlow m r) => ProductInstance.ProductInstance -> m API.ConfirmOrder
mkOnConfirmPayload orderPI = do
  trip <- BP.mkTrip orderPI
  searchPiId <- orderPI.parentId & fromMaybeM (PIFieldNotPresent "parentId")
  order <- ExternalAPITransform.mkOrder searchPiId (Just trip) Nothing
  return $ API.ConfirmOrder order
