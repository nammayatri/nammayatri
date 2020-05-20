{-# LANGUAGE OverloadedLabels #-}

module Product.Case.CRUD where

import Beckn.Types.API.Search
import Beckn.Types.App
import Beckn.Types.Common as BC
import Beckn.Types.Core.Catalog
import Beckn.Types.Core.Category
import Beckn.Types.Core.Context
import Beckn.Types.Core.Item
import Beckn.Types.Core.Price
import Beckn.Types.Mobility.Service
import Beckn.Types.Storage.Case as Case
import Beckn.Types.Storage.CaseProduct as CaseP
import Beckn.Types.Storage.Location as Location
import qualified Beckn.Types.Storage.Person as SP
import Beckn.Types.Storage.Products as Product
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import qualified Data.Accessor as Lens
import Data.Aeson
import qualified Data.Text as T
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude
import External.Gateway.Flow as Gateway
import External.Gateway.Transform as GT
import Servant
import Storage.Queries.Case as Case
import Storage.Queries.CaseProduct as CPQ
import Storage.Queries.Location as LQ
import qualified Storage.Queries.Person as QP
import Storage.Queries.Products as PQ
import qualified Storage.Queries.RegistrationToken as QR
import System.Environment
import qualified Test.RandomStrings as RS
import Types.API.Case
import Types.API.Registration
import qualified Utils.Defaults as Defaults

list :: Maybe Text -> CaseReq -> FlowHandler CaseListRes
list regToken CaseReq {..} = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyAuth regToken
  now <- getCurrTime
  caseList <- Case.findAllByType _limit _offset _type _status now
  locList <- LQ.findAllByLocIds (Case._fromLocationId <$> caseList) (Case._toLocationId <$> caseList)
  return $ catMaybes $ joinByIds locList <$> caseList
  where
    joinByIds locList cs =
      case find (\x -> (Case._fromLocationId cs == _getLocationId (Location._id x))) locList of
        Just k -> buildResponse k
        Nothing -> Nothing
      where
        buildResponse k = (prepare cs k) <$> find (\x -> (Case._toLocationId cs == _getLocationId (Location._id x))) locList
        prepare cs from to =
          CaseRes
            { _case = cs,
              _fromLocation = from,
              _toLocation = to
            }

-- Update Case
-- Transporter Accepts a Ride with Quote
-- TODO fromLocation toLocation getCreatedTimeFromInput
update :: Maybe Text -> Text -> UpdateCaseReq -> FlowHandler Case
update regToken caseId UpdateCaseReq {..} = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyAuth regToken
  person <- QP.findPersonById (PersonId _EntityId)
  c <- Case.findById $ CaseId caseId
  case (SP._organizationId person) of
    Just orgId -> case _transporterChoice of
      "ACCEPTED" -> do
        p <- createProduct c _quote Defaults.localTime orgId
        cp <- createCaseProduct c p
        notifyGateway c
        return c
      "DECLINED" -> return c
    Nothing -> L.throwException $ err400 {errBody = "ORG_ID MISSING"}

createProduct :: Case -> Maybe Double -> LocalTime -> Text -> L.Flow Products
createProduct cs price ctime orgId = do
  prodId <- L.generateGUID
  (currTime :: LocalTime) <- getCurrTime
  shortId <- L.runIO $ RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16
  let product = getProduct prodId price cs ctime currTime orgId shortId
  PQ.create product
  return $ product
  where
    getProduct prodId price cs ctime currTime orgId shortId =
      Products
        { _id = ProductsId prodId,
          _shortId = T.pack shortId,
          _name = Case._name cs,
          _description = Case._description cs,
          _industry = Case._industry cs,
          _type = RIDE,
          _status = Product.INSTOCK,
          _startTime = Case._startTime cs,
          _endTime = Case._endTime cs,
          _validTill = Case._validTill cs,
          _price = fromMaybe 0 price,
          _rating = Nothing,
          _review = Nothing,
          _udf1 = Case._udf1 cs,
          _udf2 = Case._udf2 cs,
          _udf3 = Case._udf3 cs,
          _udf4 = Case._udf4 cs,
          _udf5 = Case._udf5 cs,
          _info = Case._info cs,
          _organizationId = orgId,
          _createdAt = ctime,
          _updatedAt = currTime,
          _fromLocation = Just (Case._fromLocationId cs),
          _toLocation = Just (Case._toLocationId cs),
          _assignedTo = Nothing
        }

createCaseProduct :: Case -> Products -> L.Flow CaseProduct
createCaseProduct cs prod = do
  cpId <- L.generateGUID
  (currTime :: LocalTime) <- getCurrTime
  let caseProd = getCaseProd cpId cs prod currTime
  CPQ.create caseProd
  return $ caseProd
  where
    getCaseProd cpId cs prod currTime =
      CaseProduct
        { _id = CaseProductId cpId,
          _caseId = Case._id cs,
          _productId = Product._id prod,
          _personId = Nothing,
          _quantity = 1,
          _price = Product._price prod,
          _status = CaseP.INSTOCK,
          _info = Nothing,
          _createdAt = Case._createdAt cs,
          _updatedAt = currTime
        }

notifyGateway :: Case -> L.Flow ()
notifyGateway c = do
  L.logInfo "notifyGateway" $ show c
  cps <- CPQ.findAllByCaseId (c ^. #_id)
  L.logInfo "notifyGateway" $ show cps
  prods <- PQ.findAllById $ (\cp -> (cp ^. #_productId)) <$> cps
  onSearchPayload <- mkOnSearchPayload c prods
  L.logInfo "notifyGateway Request" $ show onSearchPayload
  Gateway.onSearch onSearchPayload
  return ()

mkOnSearchPayload :: Case -> [Products] -> L.Flow OnSearchReq
mkOnSearchPayload c prods = do
  currTime <- getCurrTime
  let context =
        Context
          { domain = "MOBILITY",
            action = "SEARCH",
            version = Just $ "0.1",
            transaction_id = c ^. #_shortId, -- TODO : What should be the txnId
            message_id = Nothing,
            timestamp = currTime,
            dummy = ""
          }
  service <- GT.mkServiceOffer c prods Nothing
  return
    OnSearchReq
      { context,
        message = service
      }
