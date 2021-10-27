{-# LANGUAGE OverloadedLabels #-}

module Product.Search where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common hiding (id)
import qualified Beckn.Types.Core.API.Search as Search
import Beckn.Types.Core.Ack
import Beckn.Types.Core.DecimalValue (convertDecimalValueToAmount)
import qualified Beckn.Types.Core.Item as Core
import qualified Beckn.Types.Core.Migration.API.Search as Core9
import qualified Beckn.Types.Core.Migration.API.Types as Core9
import qualified Beckn.Types.Core.Migration.Context as Core9
import Beckn.Types.Core.Tag
import Beckn.Types.Id
import Beckn.Types.Mobility.Catalog as BM
import Beckn.Types.Mobility.Stop as BS
import Beckn.Utils.Logging
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import EulerHS.Prelude hiding (id)
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Product.Location as Location (getDistance)
import Product.MetroOffer (buildContextMetro)
import Product.Serviceability
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.Case as QCase
import qualified Storage.Queries.Organization as Org
import qualified Storage.Queries.ProductInstance as MPI
import qualified Storage.Queries.ProductInstance as QPI
import qualified Storage.Queries.Products as QProducts
import qualified Storage.Queries.SearchReqLocation as Location
import qualified Types.API.Case as API
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
  validateDateTime req
  validateServiceability req
  fromLocation <- buildSearchReqLoc $ toBeckn $ req.origin
  toLocation <- buildSearchReqLoc $ toBeckn $ req.destination
  case_ <- mkCase req (getId personId) fromLocation toLocation
  Metrics.incrementCaseCount Case.NEW Case.RIDESEARCH
  let txnId = getId (case_.id)
  Metrics.startSearchMetrics txnId
  DB.runSqlDBTransaction $ do
    Location.create fromLocation
    Location.create toLocation
    QCase.create case_
  bapNwAddr <- asks (.nwAddress)
  context <- buildContext "search" txnId (Just bapNwAddr) Nothing
  contextMig <- buildContextMetro Core9.SEARCH txnId bapNwAddr Nothing
  let intent = mkIntent req
      tags = Just [Tag "distance" (fromMaybe "" $ case_.udf5)]
  intentMig <- mkIntentMig req
  fork "search" . withRetry $ do
    fork "search 0.8" $
      ExternalAPI.search (Search.SearchReq context $ Search.SearchIntent (intent & #tags .~ tags))
    fork "search 0.9" $
      ExternalAPI.searchMetro (Core9.BecknReq contextMig $ Core9.SearchIntent intentMig)
  return $ API.SearchRes txnId
  where
    validateDateTime sreq = do
      currTime <- getCurrentTime
      let allowedStartTime = addUTCTime (-2 * 60) currTime
      when ((sreq.origin.departureTime.estimated) < allowedStartTime) $
        throwError $ InvalidRequest "Invalid start time."
    validateServiceability sreq = do
      originGps <-
        sreq.origin.location.gps
          & fromMaybeM (InvalidRequest "GPS coordinates required for the origin location")
      destinationGps <-
        req.destination.location.gps
          & fromMaybeM (InvalidRequest "GPS coordinates required for the destination location")
      let serviceabilityReq = RideServiceabilityReq originGps destinationGps
      unlessM (rideServiceable serviceabilityReq) $
        throwError $ ProductNotServiceable "due to georestrictions"

searchCb ::
  SignatureAuthResult Org.Organization ->
  SignatureAuthResult Org.Organization ->
  Search.OnSearchReq ->
  FlowHandler Search.OnSearchRes
searchCb _ _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  validateContext "on_search" $ req.context
  Metrics.finishSearchMetrics $ req.context.transaction_id
  case req.contents of
    Right msg -> do
      let catalog = msg.catalog
      searchCbService req catalog
    Left err -> logTagError "on_search req" $ "on_search error: " <> show err
  return Ack

type SearchCbFlow m r = (HasFlowEnv m r '["searchConfirmExpiry" ::: Maybe Seconds], DBFlow m r)

searchCbService :: SearchCbFlow m r => Search.OnSearchReq -> BM.Catalog -> m ()
searchCbService req catalog = do
  let caseId = Id $ req.context.transaction_id --CaseId $ service.id
  case_ <- Case.findByIdAndType caseId Case.RIDESEARCH >>= fromMaybeM CaseDoesNotExist
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
    piList <- MPI.findAllByCaseId (case_.id)
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
  m Case.Case
mkCase req userId from to = do
  now <- getCurrentTime
  cid <- generateGUID
  distance <- Location.getDistance (req.origin.location) (req.destination.location)
  orgs <- Org.listOrganizations Nothing Nothing [Org.PROVIDER] [Org.APPROVED]
  let info = encodeToText $ API.CaseInfo (Just $ toInteger $ length orgs) (Just 0) (Just 0)
  -- TODO: consider collision probability for shortId
  -- Currently it's a random 10 char alphanumeric string
  -- If the insert fails, maybe retry automatically as there
  -- is a unique constraint on `shortId`
  shortId_ <- generateShortId
  validTill <- getCaseExpiry $ req.origin.departureTime.estimated
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
        startTime = req.origin.departureTime.estimated,
        endTime = Nothing,
        validTill = validTill,
        provider = Nothing,
        providerType = Nothing,
        requestor = Just userId,
        requestorType = Just Case.CONSUMER,
        parentCaseId = Nothing,
        fromLocationId = from.id,
        toLocationId = to.id,
        udf1 = Nothing,
        udf2 = Just . show . length $ req.travellers,
        udf3 = Nothing,
        udf4 = Just $ req.transaction_id,
        udf5 = show <$> distance,
        info = Just info,
        createdAt = now,
        updatedAt = now
      }
  where
    getCaseExpiry :: (HasFlowEnv m r '["searchCaseExpiry" ::: Maybe Seconds]) => UTCTime -> m UTCTime
    getCaseExpiry startTime = do
      now <- getCurrentTime
      caseExpiry <- maybe 7200 fromIntegral <$> asks (.searchCaseExpiry)
      let minExpiry = 300 -- 5 minutes
          timeToRide = startTime `diffUTCTime` now
          validTill = addUTCTime (minimum [fromInteger caseExpiry, maximum [minExpiry, timeToRide]]) now
      pure validTill

buildSearchReqLoc :: MonadFlow m => BS.Stop -> m Location.SearchReqLocation
buildSearchReqLoc BS.Stop {..} = do
  let loc = location
  now <- getCurrentTime
  locId <- generateGUID
  let mgps = loc.gps
  lat <- mgps >>= readMaybe . T.unpack . (.lat) & fromMaybeM (InvalidRequest "Lat field is not present.")
  lon <- mgps >>= readMaybe . T.unpack . (.lon) & fromMaybeM (InvalidRequest "Lon field is not present.")
  return
    Location.SearchReqLocation
      { id = locId,
        lat = lat,
        long = lon,
        district = Nothing,
        city = (.name) <$> loc.city,
        state = (.state) <$> loc.address,
        country = (.name) <$> loc.country,
        pincode = Nothing,
        address = T.decodeUtf8 . BSL.toStrict . encode <$> loc.address,
        createdAt = now,
        updatedAt = now
      }

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
      estimatedFare = convertDecimalValueToAmount =<< item.price.listed_value
      totalPrice = convertDecimalValueToAmount =<< item.totalPrice.listed_value
  vehicleVariant <- item.descriptor.code & fromMaybeM (InvalidRequest "Missing item.descriptor.code")
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
        chargeableDistance = Nothing,
        entityId = Nothing,
        estimatedFare = estimatedFare,
        fare = Nothing,
        estimatedTotalFare = totalPrice,
        totalFare = Nothing,
        discount = item.discount,
        _type = Case.RIDESEARCH,
        udf1 = getNearestDriverDist,
        udf2 = Nothing,
        udf3 = Nothing,
        udf4 = Nothing,
        udf5 = case_.udf5,
        fromLocation = Just $ case_.fromLocationId,
        toLocation = Just $ case_.toLocationId,
        info = Just $ encodeToText info,
        organizationId = bppOrg.id,
        vehicleVariant,
        createdAt = now,
        updatedAt = now
      }
  where
    getNearestDriverDist = value <$> listToMaybe (filter (\tag -> tag.key == "nearestDriverDist") item.tags)

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
        chargeableDistance = Nothing,
        entityId = Nothing,
        estimatedFare = Nothing,
        fare = Nothing,
        estimatedTotalFare = Nothing,
        totalFare = Nothing,
        discount = Nothing,
        _type = Case.RIDESEARCH,
        vehicleVariant = "",
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
