{-# LANGUAGE OverloadedLabels #-}

module Product.Confirm where

import App.Types
import Beckn.Types.API.Confirm
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Mobility.Order as BO
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as SPI
import Beckn.Utils.Common (decodeFromText, encodeToText, withFlowHandler)
import Beckn.Utils.Extra (getCurrentTimeUTC)
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.Flow as Gateway
import Servant
import qualified Storage.Queries.Case as QCase
import qualified Storage.Queries.ProductInstance as QPI
import qualified Test.RandomStrings as RS
import qualified Types.API.Confirm as API
import qualified Types.ProductInfo as Products
import Utils.Common (generateShortId)
import Utils.Routes

confirm :: Person.Person -> API.ConfirmReq -> FlowHandler AckResponse
confirm person API.ConfirmReq {..} = withFlowHandler $ do
  lt <- getCurrentTimeUTC
  case_ <- QCase.findIdByPerson person $ CaseId caseId
  when ((case_ ^. #_validTill) < lt) $
    L.throwException $
      err400 {errBody = "Case has expired"}
  orderCase_ <- mkOrderCase case_
  QCase.create orderCase_
  productInstance <- QPI.findById (ProductInstanceId productInstanceId)
  orderProductInstance <- mkOrderProductInstance (orderCase_ ^. #_id) productInstance
  QPI.create orderProductInstance
  transactionId <- L.generateGUID
  context <- buildContext "confirm" caseId
  baseUrl <- Gateway.getBaseUrl
  order <- mkOrder productInstanceId
  eres <- Gateway.confirm baseUrl $ ConfirmReq context $ ConfirmOrder order
  let ack =
        case eres of
          Left err -> Ack "confirm" ("Err: " <> show err)
          Right _ -> Ack "confirm" "Ok"
  return $ AckResponse context ack Nothing
  where
    mkOrder prodInstId = do
      now <- getCurrentTimeUTC
      return $
        BO.Order
          { _id = prodInstId,
            _state = Nothing,
            _billing = Nothing,
            _created_at = now,
            _updated_at = now,
            _trip = Nothing,
            _invoice = Nothing,
            _fulfillment = Nothing
          }

onConfirm :: OnConfirmReq -> FlowHandler AckResponse
onConfirm req = withFlowHandler $ do
  -- TODO: Verify api key here
  L.logInfo "on_confirm req" (show req)
  let trip = req ^. #message . #order . #_trip
      pid = ProductInstanceId $ req ^. #message . #order . #_id
      tracker = flip Products.Tracker Nothing <$> trip
  prdInst <- QPI.findById pid
  -- TODO: update tracking prodInfo in .info
  let mprdInfo = decodeFromText =<< (prdInst ^. #_info)
  let uInfo = (\info -> info {Products._tracker = tracker}) <$> mprdInfo
  let uPrd =
        prdInst
          { SPI._info = encodeToText <$> uInfo
          }
  productInstance <- QPI.findById pid -- TODO: can have multiple cases linked, fix this
  QCase.updateStatus (SPI._caseId productInstance) Case.INPROGRESS
  QPI.updateMultiple (_getProductInstanceId pid) uPrd
  QPI.updateStatus pid SPI.CONFIRMED
  return $ AckResponse (req ^. #context) (Ack "on_confirm" "Ok") Nothing

mkOrderCase :: Case.Case -> Flow Case.Case
mkOrderCase Case.Case {..} = do
  now <- getCurrentTimeUTC
  id <- generateGUID
  shortId <- generateShortId
  return
    Case.Case
      { _id = id,
        _name = Nothing,
        _description = Just "Case to order a Ride",
        _shortId = shortId,
        _industry = Case.MOBILITY,
        _type = Case.RIDEORDER,
        _parentCaseId = Just _id,
        _createdAt = now,
        _updatedAt = now,
        ..
      }

mkOrderProductInstance :: CaseId -> SPI.ProductInstance -> Flow SPI.ProductInstance
mkOrderProductInstance caseId prodInst = do
  now <- getCurrentTimeUTC
  id <- generateGUID
  shortId <- T.pack <$> L.runIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16)
  return
    SPI.ProductInstance
      { _id = ProductInstanceId id,
        _caseId = caseId,
        _productId = prodInst ^. #_productId,
        _personId = Nothing,
        _entityType = SPI.VEHICLE,
        _entityId = Nothing,
        _shortId = shortId,
        _quantity = 1,
        _price = prodInst ^. #_price,
        _organizationId = prodInst ^. #_organizationId,
        _fromLocation = prodInst ^. #_fromLocation,
        _toLocation = prodInst ^. #_toLocation,
        _startTime = prodInst ^. #_startTime,
        _endTime = prodInst ^. #_endTime,
        _validTill = prodInst ^. #_validTill,
        _parentId = Just (prodInst ^. #_id),
        _status = SPI.INSTOCK,
        _info = Nothing,
        _createdAt = now,
        _updatedAt = now,
        _udf1 = prodInst ^. #_udf1,
        _udf2 = prodInst ^. #_udf2,
        _udf3 = prodInst ^. #_udf3,
        _udf4 = prodInst ^. #_udf4,
        _udf5 = prodInst ^. #_udf5
      }
