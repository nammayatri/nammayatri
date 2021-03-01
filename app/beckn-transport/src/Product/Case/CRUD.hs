{-# LANGUAGE OverloadedLabels #-}

module Product.Case.CRUD where

import App.Types
import Beckn.Types.Amount
import Beckn.Types.App
import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.API.Search
import Beckn.Types.Core.Context
import Beckn.Types.Core.Domain as Domain
import Beckn.Types.ID
import Beckn.Types.Storage.Case as Case
import Beckn.Types.Storage.Location as Location
import Beckn.Types.Storage.Organization as Organization
import Beckn.Types.Storage.ProductInstance as PI
import Beckn.Types.Storage.Products as Product
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import qualified Data.List as List
import qualified Data.Text as T
import Data.Time
import qualified EulerHS.Language as L
import EulerHS.Prelude
import External.Gateway.Flow as Gateway
import External.Gateway.Transform as GT
import Models.Case as Case
import Models.ProductInstance as MPI
import Servant.Client (BaseUrl (..))
import Storage.Queries.Location as LQ
import Storage.Queries.Organization as OQ
import qualified Storage.Queries.Person as QP
import Storage.Queries.ProductInstance as QPI
import qualified Test.RandomStrings as RS
import Types.API.Case
import qualified Utils.Defaults as Default

list :: SR.RegistrationToken -> [CaseStatus] -> CaseType -> Maybe Int -> Maybe Int -> FlowHandler CaseListRes
list SR.RegistrationToken {..} status csType limitM offsetM = withFlowHandler $ do
  person <- QP.findPersonById (ID _EntityId)
  now <- getCurrTime
  case person ^. #_organizationId of
    Just orgId -> do
      org <- OQ.findOrganizationById (OrganizationId orgId)
      when (org ^. #_status /= Organization.APPROVED) $
        throwBecknError401 "Unauthorized"
      caseList <-
        if not (org ^. #_enabled)
          then Case.findAllByTypeStatusTime limit offset csType status orgId now $ fromMaybe now (org ^. #_fromTime)
          else Case.findAllByTypeStatuses limit offset csType status orgId now
      locList <- LQ.findAllByLocIds (Case._fromLocationId <$> caseList) (Case._toLocationId <$> caseList)
      return $ catMaybes $ joinByIds locList <$> caseList
    Nothing -> throwError400 "ORG_ID MISSING"
  where
    limit = toInteger $ fromMaybe Default.limit limitM
    offset = toInteger $ fromMaybe Default.offset offsetM
    joinByIds locList cs =
      find (\x -> Case._fromLocationId cs == _getLocationId (Location._id x)) locList
        >>= buildResponse
      where
        buildResponse k = prepare cs k <$> find (\x -> Case._toLocationId cs == _getLocationId (Location._id x)) locList
        prepare pcs from to =
          CaseRes
            { _case = pcs,
              _fromLocation = from,
              _toLocation = to
            }

createProductInstance :: Case -> Products -> Maybe Amount -> Text -> PI.ProductInstanceStatus -> Flow ProductInstance
createProductInstance cs prod price orgId status = do
  piId <- L.generateGUID
  (currTime :: UTCTime) <- getCurrTime
  shortId <- L.runIO $ RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16
  let productInst = getProdInst piId shortId currTime
  QPI.create productInst
  return productInst
  where
    getProdInst piId shortId currTime =
      ProductInstance
        { _id = ProductInstanceId piId,
          _caseId = Case._id cs,
          _productId = Product._id prod,
          _personId = Nothing,
          _personUpdatedAt = Nothing,
          _shortId = T.pack shortId,
          _entityType = PI.VEHICLE,
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

notifyGateway :: Case -> ProductInstance -> Text -> PI.ProductInstanceStatus -> Text -> Flow ()
notifyGateway c prodInst transporterOrgId piStatus bppShortId = do
  logInfo "notifyGateway" $ show c
  logInfo "notifyGateway" $ show prodInst
  transporterOrg <- OQ.findOrganizationById (OrganizationId transporterOrgId)
  onSearchPayload <- case piStatus of
    PI.OUTOFSTOCK -> mkOnSearchPayload c [] transporterOrg
    _ -> mkOnSearchPayload c [prodInst] transporterOrg
  logInfo "notifyGateway Request" $ show onSearchPayload
  _ <- Gateway.onSearch onSearchPayload bppShortId
  return ()

mkOnSearchPayload :: Case -> [ProductInstance] -> Organization -> Flow OnSearchReq
mkOnSearchPayload c pis orgInfo = do
  currTime <- getCurrTime
  appEnv <- ask
  let context =
        Context
          { _domain = Domain.MOBILITY,
            _country = Just "IND",
            _city = Nothing,
            _action = "on_search",
            _core_version = Just "0.8.2",
            _domain_version = Just "0.8.2",
            _transaction_id = last $ T.split (== '_') $ c ^. #_shortId,
            _message_id = c ^. #_shortId,
            _bap_uri = Nothing,
            _bpp_uri = Just $ makeBppUrl $ nwAddress appEnv,
            _timestamp = currTime,
            _ttl = Nothing
          }
  piCount <- MPI.getCountByStatus (_getOrganizationId $ orgInfo ^. #_id) Case.RIDEORDER
  let stats = mkProviderStats piCount
      provider = mkProviderInfo orgInfo stats
  catalog <- GT.mkCatalog c pis provider
  return
    CallbackReq
      { context,
        contents = Right $ OnSearchServices catalog
      }
  where
    makeBppUrl url =
      let orgId = _getOrganizationId $ orgInfo ^. #_id
          newPath = baseUrlPath url <> "/" <> T.unpack orgId
       in url {baseUrlPath = newPath}

-- Utility Functions

mkProviderInfo :: Organization -> ProviderStats -> ProviderInfo
mkProviderInfo org stats =
  ProviderInfo
    { _id = _getOrganizationId $ org ^. #_id,
      _name = org ^. #_name,
      _stats = encodeToText stats,
      _contacts = fromMaybe "" (org ^. #_mobileNumber)
    }

mkProviderStats :: [(PI.ProductInstanceStatus, Int)] -> ProviderStats
mkProviderStats piCount =
  ProviderStats
    { _completed = List.lookup PI.COMPLETED piCount,
      _inprogress = List.lookup PI.INPROGRESS piCount,
      _confirmed = List.lookup PI.CONFIRMED piCount
    }
