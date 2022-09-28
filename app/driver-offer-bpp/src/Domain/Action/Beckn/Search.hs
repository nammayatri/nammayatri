module Domain.Action.Beckn.Search where

import Beckn.Prelude
import Beckn.Product.MapSearch.GoogleMaps (HasCoordinates (getCoordinates))
import qualified Beckn.Product.MapSearch.GoogleMaps as GoogleMaps
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Beckn.Types.MapSearch as MapSearch
import Beckn.Utils.Common
import Data.List
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import Domain.Types.Vehicle.Variant as Variant
import Environment
import SharedLogic.DriverPool
import SharedLogic.FareCalculator
import Storage.Queries.Person (DriverPoolResult)

data DSearchReq = DSearchReq
  { messageId :: Text,
    transactionId :: Maybe Text,
    bapId :: Text,
    bapUri :: BaseUrl,
    pickupLocation :: DLoc.SearchReqLocationAPIEntity,
    pickupTime :: UTCTime,
    dropLocation :: DLoc.SearchReqLocationAPIEntity
  }

data DSearchRes = DSearchRes
  { transporterInfo :: TransporterInfo,
    fromLocation :: DLoc.SearchReqLocation,
    toLocation :: DLoc.SearchReqLocation,
    now :: UTCTime,
    estimateList :: [EstimateItem]
  }

data EstimateItem = EstimateItem
  { vehicleVariant :: Variant.Variant,
    distanceToPickup :: Meters,
    baseFare :: Money
  }

data TransporterInfo = TransporterInfo
  { shortId :: ShortId DOrg.Organization,
    name :: Text,
    contacts :: Text,
    ridesInProgress :: Int,
    ridesCompleted :: Int,
    ridesConfirmed :: Int
  }

handler :: DOrg.Organization -> DSearchReq -> Flow DSearchRes
handler org sReq = do
  fromLocation <- buildSearchReqLocation sReq.pickupLocation
  toLocation <- buildSearchReqLocation sReq.dropLocation
  driverPool <- calculateDriverPool Nothing (getCoordinates fromLocation) org.id True
  let getVariant x = x.origin.vehicle.variant
      listOfProtoQuotes = nubBy ((==) `on` getVariant) driverPool

  distRes <- GoogleMaps.getDistance (Just MapSearch.CAR) (getCoordinates fromLocation) (getCoordinates toLocation) Nothing
  let distance = distRes.distance
  logDebug $ "distance: " <> show distance
  estimates <- mapM (mkEstimate org sReq.pickupTime distance) listOfProtoQuotes
  logDebug $ "bap uri: " <> show sReq.bapUri
  buildSearchRes org fromLocation toLocation estimates

mkEstimate ::
  (MonadFlow m, Esq.Transactionable m) =>
  DOrg.Organization ->
  UTCTime ->
  Meters ->
  GoogleMaps.GetDistanceResult DriverPoolResult a ->
  m EstimateItem
mkEstimate org startTime dist g = do
  let variant = g.origin.vehicle.variant
  fareParams <- calculateFare org.id variant dist startTime Nothing
  let baseFare = fareSum fareParams
  logDebug $ "baseFare: " <> show baseFare
  logDebug $ "distanceToPickup: " <> show g.distance
  pure
    EstimateItem
      { vehicleVariant = g.origin.vehicle.variant,
        distanceToPickup = g.distance,
        baseFare
      }

buildSearchReqLocation :: (MonadGuid m, MonadTime m) => DLoc.SearchReqLocationAPIEntity -> m DLoc.SearchReqLocation
buildSearchReqLocation DLoc.SearchReqLocationAPIEntity {..} = do
  id <- Id <$> generateGUID
  now <- getCurrentTime
  let createdAt = now
      updatedAt = now
  pure DLoc.SearchReqLocation {..}

buildSearchRes ::
  (MonadTime m) =>
  DOrg.Organization ->
  DLoc.SearchReqLocation ->
  DLoc.SearchReqLocation ->
  [EstimateItem] ->
  m DSearchRes
buildSearchRes org fromLocation toLocation estimateList = do
  now <- getCurrentTime
  let transporterInfo =
        TransporterInfo
          { shortId = org.shortId,
            name = org.name,
            contacts = fromMaybe "" org.mobileNumber,
            ridesInProgress = 0, -- FIXME
            ridesCompleted = 0, -- FIXME
            ridesConfirmed = 0 -- FIXME
          }
  pure $
    DSearchRes
      { transporterInfo,
        now,
        fromLocation,
        toLocation,
        estimateList
      }
