{-# LANGUAGE OverloadedLabels #-}

module Product.Case.CRUD where

import App.Types
import Beckn.Types.API.Search
import Beckn.Types.App as BC
import Beckn.Types.Core.Amount
import Beckn.Types.Core.Catalog
import Beckn.Types.Core.Category
import Beckn.Types.Core.Context
import Beckn.Types.Core.Item
import Beckn.Types.Core.Price
import Beckn.Types.Mobility.Service
import Beckn.Types.Storage.Case as Case
import Beckn.Types.Storage.Location as Location
import Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Person as SP
import Beckn.Types.Storage.ProductInstance as ProdInst
import Beckn.Types.Storage.Products as Product
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import Beckn.Utils.Extra
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
import Storage.Queries.Location as LQ
import Storage.Queries.Organization as OQ
import qualified Storage.Queries.Organization as OQ
import qualified Storage.Queries.Person as QP
import Storage.Queries.ProductInstance as QPI
import Storage.Queries.Products as PQ
import qualified Storage.Queries.RegistrationToken as QR
import System.Environment
import qualified Test.RandomStrings as RS
import Types.API.Case
import qualified Types.API.ProductInstance as CPR
import Types.API.Registration
import qualified Utils.Defaults as Default

list :: SR.RegistrationToken -> [CaseStatus] -> CaseType -> Maybe Int -> Maybe Int -> Maybe Bool -> FlowHandler CaseListRes
list SR.RegistrationToken {..} status csType limitM offsetM ignoreOffered = withFlowHandler $ do
  person <- QP.findPersonById (PersonId _EntityId)
  now <- getCurrentTimeUTC
  case person ^. #_organizationId of
    Just orgId -> do
      org <- OQ.findOrganizationById (OrganizationId orgId)
      ignoreList <-
        if ignoreOffered == Just True
          then do
            resList <- QPI.productInstanceJoinWithoutLimits csType orgId []
            let csIgnoreList = Case._id <$> (CPR._case <$> resList)
            return csIgnoreList
          else return []
      caseList <-
        if not (org ^. #_enabled)
          then Case.findAllByTypeStatusTime limit offset csType status ignoreList now $ fromMaybe now (org ^. #_fromTime)
          else Case.findAllByTypeStatuses limit offset csType status ignoreList now
      locList <- LQ.findAllByLocIds (Case._fromLocationId <$> caseList) (Case._toLocationId <$> caseList)
      return $ catMaybes $ joinByIds locList <$> caseList
    Nothing -> L.throwException $ err400 {errBody = "ORG_ID MISSING"}
  where
    limit = toInteger $ fromMaybe Default.limit limitM
    offset = toInteger $ fromMaybe Default.offset offsetM
    joinByIds locList cs =
      find (\x -> Case._fromLocationId cs == _getLocationId (Location._id x)) locList
        >>= buildResponse
      where
        buildResponse k = prepare cs k <$> find (\x -> Case._toLocationId cs == _getLocationId (Location._id x)) locList
        prepare cs from to =
          CaseRes
            { _case = cs,
              _fromLocation = from,
              _toLocation = to
            }

-- Update Case
-- Transporter Accepts a Ride with Quote
-- TODO fromLocation toLocation getCreatedTimeFromInput
update :: SR.RegistrationToken -> Text -> UpdateCaseReq -> FlowHandler Case
update SR.RegistrationToken {..} caseId UpdateCaseReq {..} = withFlowHandler $ do
  person <- QP.findPersonById (PersonId _EntityId)
  c <- Case.findById $ CaseId caseId
  p <- PQ.findByName $ fromMaybe "DONT MATCH" (c ^. #_udf1)
  case SP._organizationId person of
    Just orgId -> case _transporterChoice of
      "ACCEPTED" -> do
        pi <- createProductInstance c p _quote orgId ProdInst.INSTOCK
        notifyGateway c pi orgId
        return c
      "DECLINED" -> do
        pi <- createProductInstance c p _quote orgId ProdInst.OUTOFSTOCK
        return c
    Nothing -> L.throwException $ err400 {errBody = "ORG_ID MISSING"}

createProductInstance :: Case -> Products -> Maybe Amount -> Text -> ProdInst.ProductInstanceStatus -> Flow ProductInstance
createProductInstance cs prod price orgId status = do
  piId <- L.generateGUID
  (currTime :: LocalTime) <- getCurrentTimeUTC
  shortId <- L.runIO $ RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16
  let productInst = getProdInst piId shortId cs prod price orgId currTime
  QPI.create productInst
  return productInst
  where
    getProdInst piId shortId cs prod price orgId currTime =
      ProductInstance
        { _id = ProductInstanceId piId,
          _caseId = Case._id cs,
          _productId = Product._id prod,
          _personId = Nothing,
          _shortId = T.pack shortId,
          _entityType = ProdInst.VEHICLE,
          _entityId = Nothing,
          _quantity = 1,
          _type = Case.RIDESEARCH,
          _price = fromMaybe 0 price,
          _status = status,
          _startTime = Case._startTime cs,
          _endTime = Case._endTime cs,
          _validTill = Case._validTill cs,
          _fromLocation = Just (Case._fromLocationId cs),
          _toLocation = Just (Case._toLocationId cs),
          _organizationId = orgId,
          _parentId = Nothing,
          _udf1 = Case._udf1 cs,
          _udf2 = Case._udf2 cs,
          _udf3 = Case._udf3 cs,
          _udf4 = Case._udf4 cs,
          _udf5 = Case._udf5 cs,
          _info = Case._info cs,
          _createdAt = currTime,
          _updatedAt = currTime
        }

notifyGateway :: Case -> ProductInstance -> Text -> Flow ()
notifyGateway c pi orgId = do
  L.logInfo "notifyGateway" $ show c
  allPis <- QPI.findAllByCaseId (c ^. #_id)
  L.logInfo "notifyGateway" $ show pi
  orgInfo <- OQ.findOrganizationById (OrganizationId orgId)
  onSearchPayload <- mkOnSearchPayload c [pi] allPis orgInfo
  L.logInfo "notifyGateway Request" $ show onSearchPayload
  Gateway.onSearch onSearchPayload
  return ()

mkOnSearchPayload :: Case -> [ProductInstance] -> [ProductInstance] -> Organization -> Flow OnSearchReq
mkOnSearchPayload c pis allPis orgInfo = do
  currTime <- getCurrentTimeUTC
  let context =
        Context
          { _domain = "MOBILITY",
            _action = "SEARCH",
            _version = Just "0.1",
            _transaction_id = c ^. #_shortId, -- TODO : What should be the txnId
            _session_id = Nothing,
            _token = Nothing,
            _timestamp = currTime,
            _status = Nothing
          }
  service <- GT.mkServiceOffer c pis allPis (Just orgInfo)
  return
    OnSearchReq
      { context,
        message = OnSearchServices [service]
      }
