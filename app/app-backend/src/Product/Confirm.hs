module Product.Confirm (confirm, onConfirm) where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common hiding (id)
import qualified Beckn.Types.Core.API.Confirm as BecknAPI
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Order (OrderItem (..))
import Beckn.Types.Id
import qualified Beckn.Types.Mobility.Order as BO
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Storage.Queries.Organization as OQ
import qualified Storage.Queries.ProductInstance as MPI
import qualified Storage.Queries.ProductInstance as QPI
import qualified Storage.Queries.SearchRequest as QSearchRequest
import qualified Test.RandomStrings as RS
import qualified Types.API.Confirm as API
import Types.Error
import qualified Types.ProductInfo as Products
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.Person as Person
import qualified Types.Storage.ProductInstance as SPI
import qualified Types.Storage.SearchRequest as SearchRequest
import Utils.Common
import qualified Utils.Metrics as Metrics

confirm :: Id Person.Person -> Id SearchRequest.SearchRequest -> Id SPI.ProductInstance -> FlowHandler API.ConfirmRes
confirm personId searchRequestId rideBookingId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  lt <- getCurrentTime
  searchRequest <- QSearchRequest.findByPersonId personId searchRequestId >>= fromMaybeM SearchRequestDoesNotExist
  when ((searchRequest.validTill) < lt) $
    throwError SearchRequestExpired
  productInstance <- MPI.findById rideBookingId >>= fromMaybeM PIDoesNotExist
  organization <-
    OQ.findOrganizationById (productInstance.organizationId)
      >>= fromMaybeM OrgNotFound
  Metrics.incrementSearchRequestCount SearchRequest.INPROGRESS SearchRequest.RIDESEARCH
  orderProductInstance <- mkOrderProductInstance (searchRequest.id) productInstance
  DB.runSqlDBTransaction $ do
    QPI.create orderProductInstance
  context <- buildContext "confirm" (getId searchRequestId) Nothing Nothing
  baseUrl <- organization.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
  order <- mkOrder productInstance
  ExternalAPI.confirm baseUrl (BecknAPI.ConfirmReq context $ BecknAPI.ConfirmOrder order)
  return $ API.ConfirmRes orderProductInstance.id
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

onConfirm ::
  SignatureAuthResult Organization.Organization ->
  BecknAPI.OnConfirmReq ->
  FlowHandler AckResponse
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
        prdInst <- MPI.findById pid >>= fromMaybeM PIDoesNotExist
        -- TODO: update tracking prodInfo in.info
        let mprdInfo = decodeFromText =<< (prdInst.info)
        let uInfo = (\info -> info {Products.tracker = tracker}) <$> mprdInfo
        let uPrd =
              prdInst
                { SPI.info = encodeToText <$> uInfo,
                  SPI.udf4 = (.id) <$> trip,
                  SPI.status = SPI.CONFIRMED
                }
        Metrics.incrementSearchRequestCount SearchRequest.COMPLETED SearchRequest.RIDESEARCH
        SPI.validateStatusTransition (SPI.status prdInst) SPI.CONFIRMED & fromEitherM PIInvalidStatus
        DB.runSqlDBTransaction $ do
          QPI.updateMultiple pid uPrd
      Left err -> logTagError "on_confirm req" $ "on_confirm error: " <> show err
    return Ack

mkOrderProductInstance :: MonadFlow m => Id SearchRequest.SearchRequest -> SPI.ProductInstance -> m SPI.ProductInstance
mkOrderProductInstance searchRequestId prodInst@SPI.ProductInstance {..} = do
  now <- getCurrentTime
  piid <- generateGUID
  shortId' <- T.pack <$> L.runIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16)
  return
    SPI.ProductInstance
      { id = Id piid,
        requestId = searchRequestId,
        entityType = SPI.VEHICLE,
        entityId = Nothing,
        shortId = ShortId shortId',
        quantity = 1,
        _type = SPI.RIDEORDER,
        parentId = Just (prodInst.id),
        status = SPI.INSTOCK,
        createdAt = now,
        updatedAt = now,
        ..
      }
