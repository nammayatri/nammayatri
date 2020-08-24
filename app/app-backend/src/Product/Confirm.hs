{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Confirm (confirm, onConfirm) where

import App.Types
import Beckn.Types.API.Confirm
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Order (OrderItem (..))
import qualified Beckn.Types.Mobility.Order as BO
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as SPI
import Beckn.Utils.Common (decodeFromText, encodeToText, getCurrTime, withFlowHandler)
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.Flow as Gateway
import qualified Models.Case as MC
import qualified Models.Case as QCase
import qualified Models.ProductInstance as MPI
import Servant
import qualified Test.RandomStrings as RS
import qualified Types.API.Confirm as API
import Types.Common (fromBeckn)
import qualified Types.ProductInfo as Products
import Utils.Common (generateShortId)
import qualified Utils.Metrics as Metrics
import Utils.Routes

confirm :: Person.Person -> API.ConfirmReq -> FlowHandler AckResponse
confirm person API.ConfirmReq {..} = withFlowHandler $ do
  lt <- getCurrTime
  case_ <- QCase.findIdByPerson person $ CaseId caseId
  when ((case_ ^. #_validTill) < lt) $
    L.throwException $
      err400 {errBody = "Case has expired"}
  orderCase_ <- mkOrderCase case_
  Metrics.incrementCaseCount Case.INPROGRESS Case.RIDEORDER
  QCase.create orderCase_
  productInstance <- MPI.findById (ProductInstanceId productInstanceId)
  orderProductInstance <- mkOrderProductInstance (orderCase_ ^. #_id) productInstance
  MPI.create orderProductInstance
  context <- buildContext "confirm" caseId
  baseUrl <- xProviderUri <$> ask
  order <- mkOrder productInstance
  Gateway.confirm baseUrl $ ConfirmReq context $ ConfirmOrder order
  where
    mkOrder productInstance = do
      now <- getCurrTime
      return $
        BO.Order
          { _id = _getProductInstanceId $ productInstance ^. #_id,
            _state = Nothing,
            _created_at = now,
            _updated_at = now,
            _items = [OrderItem (_getProductsId $ productInstance ^. #_productId) Nothing],
            _billing = Nothing,
            _payment = Nothing,
            _trip = Nothing
          }

onConfirm :: OnConfirmReq -> FlowHandler AckResponse
onConfirm req = withFlowHandler $ do
  -- TODO: Verify api key here
  L.logInfo @Text "on_confirm req" (show req)
  case req ^. #contents of
    Right msg -> do
      let trip = fromBeckn <$> msg ^. #order . #_trip
          pid = ProductInstanceId $ msg ^. #order . #_id
          tracker = flip Products.Tracker Nothing <$> trip
      prdInst <- MPI.findById pid
      -- TODO: update tracking prodInfo in .info
      let mprdInfo = decodeFromText =<< (prdInst ^. #_info)
      let uInfo = (\info -> info {Products._tracker = tracker}) <$> mprdInfo
      let uPrd =
            prdInst
              { SPI._info = encodeToText <$> uInfo
              }
      productInstance <- MPI.findById pid -- TODO: can have multiple cases linked, fix this
      Metrics.incrementCaseCount Case.COMPLETED Case.RIDEORDER
      MC.updateStatus (SPI._caseId productInstance) Case.COMPLETED
      MPI.updateMultiple pid uPrd
      MPI.updateStatus pid SPI.CONFIRMED
    Left err -> L.logError @Text "on_confirm req" $ "on_confirm error: " <> show err
  return $ AckResponse (req ^. #context) (ack "ACK") Nothing

mkOrderCase :: Case.Case -> Flow Case.Case
mkOrderCase Case.Case {..} = do
  now <- getCurrTime
  caseId <- generateGUID
  shortId <- generateShortId
  return
    Case.Case
      { _id = caseId,
        _name = Nothing,
        _description = Just "Case to order a Ride",
        _shortId = shortId,
        _status = Case.INPROGRESS,
        _industry = Case.MOBILITY,
        _type = Case.RIDEORDER,
        _parentCaseId = Just _id,
        _fromLocationId = _fromLocationId,
        _toLocationId = _toLocationId,
        _startTime = _startTime,
        _requestor = _requestor,
        _createdAt = now,
        _updatedAt = now,
        ..
      }

mkOrderProductInstance :: CaseId -> SPI.ProductInstance -> Flow SPI.ProductInstance
mkOrderProductInstance caseId prodInst = do
  now <- getCurrTime
  piid <- generateGUID
  shortId <- T.pack <$> L.runIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16)
  return
    SPI.ProductInstance
      { _id = ProductInstanceId piid,
        _caseId = caseId,
        _productId = prodInst ^. #_productId,
        _personId = prodInst ^. #_personId,
        _entityType = SPI.VEHICLE,
        _entityId = Nothing,
        _shortId = shortId,
        _quantity = 1,
        _price = prodInst ^. #_price,
        _type = Case.RIDEORDER,
        _organizationId = prodInst ^. #_organizationId,
        _fromLocation = prodInst ^. #_fromLocation,
        _toLocation = prodInst ^. #_toLocation,
        _startTime = prodInst ^. #_startTime,
        _endTime = prodInst ^. #_endTime,
        _validTill = prodInst ^. #_validTill,
        _parentId = Just (prodInst ^. #_id),
        _status = SPI.INSTOCK,
        _info = prodInst ^. #_info,
        _createdAt = now,
        _updatedAt = now,
        _udf1 = prodInst ^. #_udf1,
        _udf2 = prodInst ^. #_udf2,
        _udf3 = prodInst ^. #_udf3,
        _udf4 = prodInst ^. #_udf4,
        _udf5 = prodInst ^. #_udf5
      }
