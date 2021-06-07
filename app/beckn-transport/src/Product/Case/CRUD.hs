{-# LANGUAGE OverloadedLabels #-}

module Product.Case.CRUD where

import App.Types
import Beckn.Types.Amount
import Beckn.Types.Common hiding (id)
import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.API.Search
import Beckn.Types.Core.Context
import Beckn.Types.Core.Domain as Domain
import Beckn.Types.Id
import Beckn.Types.Storage.Case as Case
import Beckn.Types.Storage.Location as Location
import Beckn.Types.Storage.Organization as Organization
import Beckn.Types.Storage.ProductInstance as PI
import Beckn.Types.Storage.Products as Product
import qualified Beckn.Types.Storage.RegistrationToken as SR
import qualified Data.List as List
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import ExternalAPI.Flow as ExternalAPI
import ExternalAPI.Transform as ExternalAPITransform
import Models.Case as Case
import Models.ProductInstance as MPI
import Servant.Client (BaseUrl (..))
import Storage.Queries.Location as LQ
import Storage.Queries.Organization as OQ
import qualified Storage.Queries.Person as QP
import Storage.Queries.ProductInstance as QPI
import qualified Test.RandomStrings as RS
import Types.API.Case
import Types.Error
import Utils.Common
import qualified Utils.Defaults as Default

list :: SR.RegistrationToken -> [CaseStatus] -> CaseType -> Maybe Int -> Maybe Int -> FlowHandler CaseListRes
list SR.RegistrationToken {..} status csType limitM offsetM = withFlowHandlerAPI $ do
  person <- QP.findPersonById (Id entityId)
  now <- getCurrentTime
  case person ^. #organizationId of
    Just orgId -> do
      org <- OQ.findOrganizationById (Id orgId)
      when (org ^. #status /= Organization.APPROVED) $
        throwError Unauthorized
      caseList <-
        if not (org ^. #enabled)
          then Case.findAllByTypeStatusTime limit offset csType status orgId now $ fromMaybe now (org ^. #fromTime)
          else Case.findAllByTypeStatuses limit offset csType status orgId now
      locList <- LQ.findAllByLocIds (Case.fromLocationId <$> caseList) (Case.toLocationId <$> caseList)
      return $ catMaybes $ joinByIds locList <$> caseList
    Nothing -> throwError (PersonFieldNotPresent "organization_id")
  where
    limit = toInteger $ fromMaybe Default.limit limitM
    offset = toInteger $ fromMaybe Default.offset offsetM
    joinByIds locList cs =
      find (\x -> Case.fromLocationId cs == getId (Location.id x)) locList
        >>= buildResponse
      where
        buildResponse k = prepare cs k <$> find (\x -> Case.toLocationId cs == getId (Location.id x)) locList
        prepare pcs from to =
          CaseRes
            { _case = pcs,
              fromLocation = from,
              toLocation = to
            }

createProductInstance :: Case -> Products -> Maybe Amount -> Text -> PI.ProductInstanceStatus -> Flow ProductInstance
createProductInstance cs prod price orgId status = do
  piId <- L.generateGUID
  (currTime :: UTCTime) <- getCurrentTime
  shortId <- L.runIO $ RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16
  let productInst = getProdInst piId shortId currTime
  QPI.createFlow productInst
  return productInst
  where
    getProdInst piId shortId currTime =
      ProductInstance
        { id = Id piId,
          caseId = Case.id cs,
          productId = Product.id prod,
          personId = Nothing,
          personUpdatedAt = Nothing,
          shortId = T.pack shortId,
          entityType = PI.VEHICLE,
          entityId = Nothing,
          quantity = 1,
          _type = Case.RIDESEARCH,
          price = price,
          status = status,
          startTime = Case.startTime cs,
          endTime = Case.endTime cs,
          validTill = Case.validTill cs,
          fromLocation = Just (Case.fromLocationId cs),
          toLocation = Just (Case.toLocationId cs),
          organizationId = orgId,
          parentId = Nothing,
          udf1 = Case.udf1 cs,
          udf2 = Case.udf2 cs,
          udf3 = Case.udf3 cs,
          udf4 = Case.udf4 cs,
          udf5 = Case.udf5 cs,
          info = Case.info cs,
          createdAt = currTime,
          updatedAt = currTime
        }

notifyGateway :: Case -> ProductInstance -> Text -> PI.ProductInstanceStatus -> Text -> Flow ()
notifyGateway c prodInst transporterOrgId piStatus bppShortId = do
  logTagInfo "notifyGateway" $ show c
  logTagInfo "notifyGateway" $ show prodInst
  transporterOrg <- OQ.findOrganizationById (Id transporterOrgId)
  onSearchPayload <- case piStatus of
    PI.OUTOFSTOCK -> mkOnSearchPayload c [] transporterOrg
    _ -> mkOnSearchPayload c [prodInst] transporterOrg
  logTagInfo "notifyGateway Request" $ show onSearchPayload
  ExternalAPI.onSearch onSearchPayload bppShortId

mkOnSearchPayload :: Case -> [ProductInstance] -> Organization -> Flow OnSearchReq
mkOnSearchPayload c pis orgInfo = do
  currTime <- getCurrentTime
  appEnv <- ask
  let context =
        Context
          { domain = Domain.MOBILITY,
            country = Just "IND",
            city = Nothing,
            action = "on_search",
            core_version = Just "0.8.2",
            domain_version = Just "0.8.2",
            transaction_id = last $ T.split (== '_') $ c ^. #shortId,
            message_id = c ^. #shortId,
            bap_uri = Nothing,
            bpp_uri = Just $ makeBppUrl $ nwAddress appEnv,
            timestamp = currTime,
            ttl = Nothing
          }
  piCount <- MPI.getCountByStatus (getId $ orgInfo ^. #id) Case.RIDEORDER
  let stats = mkProviderStats piCount
      provider = mkProviderInfo orgInfo stats
  catalog <- ExternalAPITransform.mkCatalog c pis provider
  return
    CallbackReq
      { context,
        contents = Right $ OnSearchServices catalog
      }
  where
    makeBppUrl url =
      let orgId = getId $ orgInfo ^. #id
          newPath = baseUrlPath url <> "/" <> T.unpack orgId
       in url {baseUrlPath = newPath}

-- Utility Functions

mkProviderInfo :: Organization -> ProviderStats -> ProviderInfo
mkProviderInfo org stats =
  ProviderInfo
    { id = getId $ org ^. #id,
      name = org ^. #name,
      stats = encodeToText stats,
      contacts = fromMaybe "" (org ^. #mobileNumber)
    }

mkProviderStats :: [(PI.ProductInstanceStatus, Int)] -> ProviderStats
mkProviderStats piCount =
  ProviderStats
    { completed = List.lookup PI.COMPLETED piCount,
      inprogress = List.lookup PI.INPROGRESS piCount,
      confirmed = List.lookup PI.CONFIRMED piCount
    }
