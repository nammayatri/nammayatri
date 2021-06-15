module Product.Confirm (confirm, onConfirm) where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.APISuccess (APISuccess (Success))
import Beckn.Types.Common hiding (id)
import qualified Beckn.Types.Core.API.Confirm as BecknAPI
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Order (OrderItem (..))
import Beckn.Types.Id
import qualified Beckn.Types.Mobility.Order as BO
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as SPI
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Models.Case as MCase
import qualified Models.ProductInstance as MPI
import qualified Storage.Queries.Case as QCase
import qualified Storage.Queries.Organization as OQ
import qualified Storage.Queries.ProductInstance as QPI
import qualified Test.RandomStrings as RS
import qualified Types.API.Confirm as API
import Types.Error
import qualified Types.ProductInfo as Products
import Utils.Common
import qualified Utils.Metrics as Metrics

confirm :: Person.Person -> API.ConfirmReq -> FlowHandler API.ConfirmRes
confirm person API.ConfirmReq {..} = withFlowHandlerAPI $ do
  lt <- getCurrentTime
  case_ <- MCase.findIdByPerson person $ Id caseId
  when ((case_.validTill) < lt) $
    throwError CaseExpired
  orderCase_ <- mkOrderCase case_
  productInstance <- MPI.findById (Id productInstanceId)
  organization <-
    OQ.findOrganizationById (productInstance.organizationId)
      >>= fromMaybeM OrgNotFound
  Metrics.incrementCaseCount Case.INPROGRESS Case.RIDEORDER
  orderProductInstance <- mkOrderProductInstance (orderCase_.id) productInstance
  DB.runSqlDBTransaction $ do
    QCase.create orderCase_
    QPI.create orderProductInstance
  context <- buildContext "confirm" caseId Nothing Nothing
  baseUrl <- organization.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  order <- mkOrder productInstance
  ExternalAPI.confirm baseUrl (BecknAPI.ConfirmReq context $ BecknAPI.ConfirmOrder order)
  return Success
  where
    mkOrder productInstance = do
      now <- getCurrentTime
      return $
        BO.Order
          { id = getId $ productInstance.id,
            state = Nothing,
            created_at = now,
            updated_at = now,
            items = [OrderItem (getId $ productInstance.productId) Nothing],
            billing = Nothing,
            payment = Nothing,
            trip = Nothing,
            cancellation_reason_id = Nothing,
            cancellation_reasons = [],
            cancellation_policy = Nothing
          }

onConfirm :: Organization.Organization -> BecknAPI.OnConfirmReq -> FlowHandler AckResponse
onConfirm _org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    -- TODO: Verify api key here
    logTagInfo "on_confirm req" (show req)
    validateContext "on_confirm" $ req.context
    case req.contents of
      Right msg -> do
        let trip = fromBeckn <$> msg.order.trip
            pid = Id $ msg.order.id
            tracker = flip Products.Tracker Nothing <$> trip
        prdInst <- MPI.findById pid
        -- TODO: update tracking prodInfo in.info
        let mprdInfo = decodeFromText =<< (prdInst.info)
        let uInfo = (\info -> info {Products.tracker = tracker}) <$> mprdInfo
        let uPrd =
              prdInst
                { SPI.info = encodeToText <$> uInfo,
                  SPI.udf4 = (.id) <$> trip,
                  SPI.status = SPI.CONFIRMED
                }
        productInstance <- MPI.findById pid
        Metrics.incrementCaseCount Case.COMPLETED Case.RIDEORDER
        let newCaseStatus = Case.COMPLETED
        case_ <- MCase.findById $ productInstance.caseId
        Case.validateStatusTransition (case_.status) newCaseStatus & fromEitherM CaseInvalidStatus
        SPI.validateStatusTransition (SPI.status productInstance) SPI.CONFIRMED & fromEitherM PIInvalidStatus
        DB.runSqlDBTransaction $ do
          QCase.updateStatus (productInstance.caseId) newCaseStatus
          QPI.updateMultiple pid uPrd
      Left err -> logTagError "on_confirm req" $ "on_confirm error: " <> show err
    return Ack

mkOrderCase :: Case.Case -> Flow Case.Case
mkOrderCase Case.Case {..} = do
  now <- getCurrentTime
  caseId <- generateGUID
  shortId_ <- generateShortId
  return
    Case.Case
      { id = caseId,
        name = Nothing,
        description = Just "Case to order a Ride",
        shortId = shortId_,
        status = Case.INPROGRESS,
        industry = Case.MOBILITY,
        _type = Case.RIDEORDER,
        parentCaseId = Just id,
        fromLocationId = fromLocationId,
        toLocationId = toLocationId,
        startTime = startTime,
        requestor = requestor,
        createdAt = now,
        updatedAt = now,
        ..
      }

mkOrderProductInstance :: Id Case.Case -> SPI.ProductInstance -> Flow SPI.ProductInstance
mkOrderProductInstance caseId prodInst = do
  now <- getCurrentTime
  piid <- generateGUID
  shortId <- T.pack <$> L.runIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16)
  return
    SPI.ProductInstance
      { id = Id piid,
        caseId = caseId,
        productId = prodInst.productId,
        personId = prodInst.personId,
        personUpdatedAt = prodInst.personUpdatedAt,
        entityType = SPI.VEHICLE,
        entityId = Nothing,
        shortId = ShortId shortId,
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
        status = SPI.INSTOCK,
        info = prodInst.info,
        createdAt = now,
        updatedAt = now,
        udf1 = prodInst.udf1,
        udf2 = prodInst.udf2,
        udf3 = prodInst.udf3,
        udf4 = prodInst.udf4,
        udf5 = prodInst.udf5
      }
