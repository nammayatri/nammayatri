{-# LANGUAGE OverloadedLabels #-}

module Product.Search where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common hiding (id)
import qualified Beckn.Types.Core.API.Search as Search
import Beckn.Types.Core.Ack
import Beckn.Types.Core.DecimalValue (convertDecimalValueToAmount)
import qualified Beckn.Types.Core.Item as Core
import Beckn.Types.Core.Location
import Beckn.Types.Core.Price
import Beckn.Types.Core.Tag
import Beckn.Types.Id
import Beckn.Types.Mobility.Catalog as BM
import Beckn.Types.Mobility.Intent
import Beckn.Types.Mobility.Payload
import Beckn.Types.Mobility.Stop
import Beckn.Types.Mobility.Vehicle
import Beckn.Utils.Logging
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Data.Map as Map
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import EulerHS.Prelude hiding (id)
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Product.Location as Location (getDistance)
import Product.Serviceability
import qualified Storage.Queries.Case as QCase
import qualified Storage.Queries.Organization as Org
import qualified Storage.Queries.ProductInstance as MPI
import qualified Storage.Queries.ProductInstance as QPI
import qualified Storage.Queries.Products as QProducts
import qualified Storage.Queries.SearchReqLocation as Location
import qualified Types.API.Search as API
import Types.API.Serviceability
import qualified Types.Common as Common
import Types.Error
import Types.Metrics (CoreMetrics)
import Types.ProductInfo
import qualified Types.Storage.Case as Case
import qualified Types.Storage.Organization as Org
import qualified Types.Storage.Person as Person
import qualified Types.Storage.ProductInstance as PI
import qualified Types.Storage.Products as Products
import qualified Types.Storage.SearchReqLocation as Location
import Utils.Common
import qualified Utils.Metrics as Metrics

search :: Id Person.Person -> API.SearchReq -> FlowHandler API.SearchRes
search personId req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  validateServiceability
  fromLocation <- Location.buildSearchReqLoc req.origin
  toLocation <- Location.buildSearchReqLoc req.destination
  now <- getCurrentTime
  case_ <- mkCase req (getId personId) fromLocation toLocation now
  Metrics.incrementCaseCount Case.NEW Case.RIDESEARCH
  let txnId = getId (case_.id)
  Metrics.startSearchMetrics txnId
  DB.runSqlDBTransaction $ do
    Location.create fromLocation
    Location.create toLocation
    QCase.create case_
  env <- ask
  let bapNwAddr = env.bapNwAddress
  context <- buildContext "search" txnId (Just bapNwAddr) Nothing
  let intent = mkIntent req now
      tags = Just [Tag "distance" (fromMaybe "" $ case_.udf5)]
  fork "search" . withRetry $
    ExternalAPI.search (xGatewayUri env) (Search.SearchReq context $ Search.SearchIntent (intent & #tags .~ tags))
  return . API.SearchRes $ case_.id
  where
    validateServiceability = do
      let originGps = req.origin.gps
      let destinationGps = req.destination.gps
      let serviceabilityReq = RideServiceabilityReq originGps destinationGps
      unlessM (rideServiceable serviceabilityReq) $
        throwError $ ProductNotServiceable "due to georestrictions"

searchCb ::
  SignatureAuthResult Org.Organization ->
  SignatureAuthResult Org.Organization ->
  Search.OnSearchReq ->
  FlowHandler Search.OnSearchRes
searchCb _ _ req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    validateContext "on_search" $ req.context
    Metrics.finishSearchMetrics $ req.context.transaction_id
    case req.contents of
      Right msg -> do
        let catalog = msg.catalog
        searchCbService req catalog
      Left err -> logTagError "on_search req" $ "on_search error: " <> show err
    return Ack

searchCbService :: (HasFlowEnv m r '["searchConfirmExpiry" ::: Maybe Seconds], DBFlow m r) => Search.OnSearchReq -> BM.Catalog -> m ()
searchCbService req catalog = do
  let caseId = Id $ req.context.transaction_id --CaseId $ service.id
  case_ <- QCase.findById caseId >>= fromMaybeM CaseDoesNotExist
  when (case_.status /= Case.CLOSED) $ do
    bpp <-
      Org.findOrganizationByCallbackUri (req.context.bpp_uri) Org.PROVIDER
        >>= fromMaybeM OrgDoesNotExist
    personId <- (Id <$> Case.requestor case_) & fromMaybeM (CaseFieldNotPresent "requestor")
    transaction <- case (catalog.categories, catalog.items) of
      ([], _) -> throwError $ InvalidRequest "Missing provider"
      (category : _, []) -> do
        let provider = fromBeckn category
        declinedPI <- mkDeclinedProductInstance case_ bpp provider personId
        return $ QPI.create declinedPI
      (category : _, items) -> do
        when
          (case_.status == Case.CLOSED)
          (throwError CaseExpired)
        let provider = fromBeckn category
        products <- traverse (mkProduct case_) items
        productInstances <- traverse (mkProductInstance case_ bpp provider personId) items
        currTime <- getCurrentTime
        confirmExpiry <- maybe 1800 fromIntegral <$> asks (.searchConfirmExpiry)
        let newValidTill = fromInteger confirmExpiry `addUTCTime` currTime
        return $ do
          traverse_ QProducts.create products
          traverse_ QPI.create productInstances
          when (case_.validTill < newValidTill) $ QCase.updateValidTill (case_.id) newValidTill
    piList <- MPI.findAllByCaseIdAndType (case_.id) PI.RIDESEARCH
    let piStatusCount = Map.fromListWith (+) $ zip (PI.status <$> piList) $ repeat (1 :: Integer)
        accepted = Map.lookup PI.INSTOCK piStatusCount
        declined = Map.lookup PI.OUTOFSTOCK piStatusCount
        mCaseInfo :: (Maybe API.CaseInfo) = decodeFromText =<< (case_.info)

    DB.runSqlDBTransaction $ do
      transaction
      whenJust mCaseInfo $ \info -> do
        let uInfo = info & #accepted .~ accepted & #declined .~ declined
        QCase.updateInfo (case_.id) (encodeToText uInfo)

mkCase ::
  ( (HasFlowEnv m r ["searchCaseExpiry" ::: Maybe Seconds, "graphhopperUrl" ::: BaseUrl]),
    DBFlow m r,
    CoreMetrics m
  ) =>
  API.SearchReq ->
  Text ->
  Location.SearchReqLocation ->
  Location.SearchReqLocation ->
  UTCTime ->
  m Case.Case
mkCase req userId from to now = do
  cid <- generateGUID
  distance <- Location.getDistance (req.origin.gps) (req.destination.gps)
  orgs <- Org.listOrganizations Nothing Nothing [Org.PROVIDER] [Org.APPROVED]
  let info = encodeToText $ API.CaseInfo (Just $ toInteger $ length orgs) (Just 0) (Just 0)
  -- TODO: consider collision probability for shortId
  -- Currently it's a random 10 char alphanumeric string
  -- If the insert fails, maybe retry automatically as there
  -- is a unique constraint on `shortId`
  shortId_ <- generateShortId
  validTill <- getCaseExpiry now
  return
    Case.Case
      { id = cid,
        name = Nothing,
        description = Just "Case to search for a Ride",
        shortId = shortId_,
        industry = Case.MOBILITY,
        _type = Case.RIDESEARCH,
        exchangeType = Case.FULFILLMENT,
        status = Case.NEW,
        startTime = now,
        endTime = Nothing,
        validTill = validTill,
        provider = Nothing,
        providerType = Nothing,
        requestor = Just userId,
        requestorType = Just Case.CONSUMER,
        fromLocationId = from.id,
        toLocationId = to.id,
        udf1 = Just . show $ req.vehicle,
        udf2 = Nothing,
        udf3 = Nothing,
        udf4 = Nothing,
        udf5 = show <$> distance,
        info = Just info,
        createdAt = now,
        updatedAt = now
      }
  where
    getCaseExpiry :: (HasFlowEnv m r '["searchCaseExpiry" ::: Maybe Seconds]) => UTCTime -> m UTCTime
    getCaseExpiry startTime = do
      caseExpiry <- maybe 7200 fromIntegral <$> asks (.searchCaseExpiry)
      let minExpiry = 300 -- 5 minutes
          timeToRide = startTime `diffUTCTime` now
          validTill = addUTCTime (minimum [fromInteger caseExpiry, maximum [minExpiry, timeToRide]]) now
      pure validTill

mkProduct :: MonadFlow m => Case.Case -> Core.Item -> m Products.Products
mkProduct case_ item = do
  now <- getCurrentTime
  price <-
    case convertDecimalValueToAmount =<< item.price.listed_value of
      Nothing -> throwError $ InvalidRequest "convertDecimalValueToAmount returns Nothing."
      Just p -> return p
  -- There is loss of data in coversion Product -> Item -> Product
  -- In api exchange between transporter and app-backend
  -- TODO: fit public transport, where case.startTime != product.startTime, etc
  return
    Products.Products
      { id = Id $ item.id,
        shortId = "",
        name = fromMaybe "" $ item.descriptor.name,
        description = item.descriptor.short_desc,
        industry = case_.industry,
        _type = Products.RIDE,
        status = Products.INSTOCK,
        price = price,
        rating = Nothing,
        review = Nothing,
        udf1 = Nothing,
        udf2 = Nothing,
        udf3 = Nothing,
        udf4 = Nothing,
        udf5 = Nothing,
        info = Nothing,
        createdAt = now,
        updatedAt = now
      }

mkProductInstance ::
  MonadFlow m =>
  Case.Case ->
  Org.Organization ->
  Common.Provider ->
  Id Person.Person ->
  Core.Item ->
  m PI.ProductInstance
mkProductInstance case_ bppOrg provider personId item = do
  now <- getCurrentTime
  let info = ProductInfo (Just provider) Nothing
      price = convertDecimalValueToAmount =<< item.price.listed_value
  -- There is loss of data in coversion Product -> Item -> Product
  -- In api exchange between transporter and app-backend
  -- TODO: fit public transport, where case.startTime != product.startTime, etc
  return
    PI.ProductInstance
      { id = Id $ item.id,
        shortId = "",
        caseId = case_.id,
        productId = Id $ item.id, -- TODO needs to be fixed
        personId = Just personId,
        personUpdatedAt = Nothing,
        quantity = 1,
        entityType = PI.VEHICLE,
        status = PI.INSTOCK,
        startTime = case_.startTime,
        endTime = case_.endTime,
        validTill = case_.validTill,
        parentId = Nothing,
        actualDistance = Nothing,
        entityId = Nothing,
        price = price,
        actualPrice = Nothing,
        _type = PI.RIDESEARCH,
        udf1 = getNearestDriverDist,
        udf2 = Nothing,
        udf3 = Nothing,
        udf4 = Nothing,
        udf5 = case_.udf5,
        fromLocation = Just $ case_.fromLocationId,
        toLocation = Just $ case_.toLocationId,
        info = Just $ encodeToText info,
        organizationId = bppOrg.id,
        createdAt = now,
        updatedAt = now
      }
  where
    getNearestDriverDist = (.value) <$> listToMaybe (filter (\tag -> tag.key == "nearestDriverDist") item.tags)

mkDeclinedProductInstance :: MonadFlow m => Case.Case -> Org.Organization -> Common.Provider -> Id Person.Person -> m PI.ProductInstance
mkDeclinedProductInstance case_ bppOrg provider personId = do
  now <- getCurrentTime
  piId <- generateGUID
  let info = ProductInfo (Just provider) Nothing
  return
    PI.ProductInstance
      { id = Id piId,
        shortId = "",
        caseId = case_.id,
        productId = Id piId,
        personId = Just personId,
        personUpdatedAt = Nothing,
        quantity = 1,
        entityType = PI.VEHICLE,
        status = PI.OUTOFSTOCK,
        startTime = case_.startTime,
        endTime = case_.endTime,
        validTill = case_.validTill,
        parentId = Nothing,
        actualDistance = Nothing,
        entityId = Nothing,
        price = Nothing,
        actualPrice = Nothing,
        _type = PI.RIDESEARCH,
        udf1 = Nothing,
        udf2 = Nothing,
        udf3 = Nothing,
        udf4 = Nothing,
        udf5 = Nothing,
        fromLocation = Just $ case_.fromLocationId,
        toLocation = Just $ case_.toLocationId,
        info = Just $ encodeToText info,
        organizationId = bppOrg.id,
        createdAt = now,
        updatedAt = now
      }

mkIntent :: API.SearchReq -> UTCTime -> Intent
mkIntent req now = do
  let pickupLocation =
        emptyLocation
          { gps = Just $ toBeckn req.origin.gps,
            address = Just $ toBeckn req.origin.address
          }
      dropLocation =
        emptyLocation
          { gps = Just $ toBeckn req.destination.gps,
            address = Just $ toBeckn req.destination.address
          }
      pickup =
        Stop
          { id = "",
            descriptor = Nothing,
            location = pickupLocation,
            arrival_time = StopTime now Nothing,
            departure_time = StopTime now Nothing,
            transfers = []
          }
      drop' =
        Stop
          { id = "",
            descriptor = Nothing,
            location = dropLocation,
            arrival_time = StopTime now Nothing,
            departure_time = StopTime now Nothing,
            transfers = []
          }
      vehicle =
        emptyVehicle
          { variant = show req.vehicle
          }
      fare = emptyPrice
  Intent
    { query_string = Nothing,
      provider_id = Nothing,
      category_id = Nothing,
      item_id = Nothing,
      tags = Nothing,
      pickups = [pickup],
      drops = [drop'],
      vehicle = vehicle,
      payload = Payload Nothing Nothing [] Nothing,
      transfer = Nothing,
      fare = fare
    }
