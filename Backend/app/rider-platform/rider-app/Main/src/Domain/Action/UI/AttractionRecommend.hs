module Domain.Action.UI.AttractionRecommend (postAttractionsRecommend, makeRecommendedTicketPlacesKey) where

import qualified API.Types.UI.AttractionRecommend as API
import qualified Data.List as L
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.TicketPlace as DTicketPlace
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Distance (HighPrecMeters (..), highPrecMetersToMeters)
import qualified Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import qualified Storage.Queries.TicketPlace as QTicketPlace

data CachedAttraction = CachedAttraction
  { id :: Kernel.Types.Id.Id DTicketPlace.TicketPlace,
    name :: Text,
    iconUrl :: Maybe Text,
    lat :: Double,
    lon :: Double
  }
  deriving (Generic, Eq, Show, Read, ToJSON, FromJSON, ToSchema, Ord)

makeRecommendedTicketPlacesKey :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Text
makeRecommendedTicketPlacesKey merchantId = "AttractionRecommend:mid-" <> merchantId.getId

postAttractionsRecommend ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.AttractionRecommendReq ->
    Environment.Flow API.AttractionRecommendResp
  )
postAttractionsRecommend (_, merchantId) req = do
  let requestedRadiusKm = req.radius
      requestedCount = req.count
      redisKey = makeRecommendedTicketPlacesKey merchantId
      userPos = LatLong req.lat req.lon
  mbCachedAttractions :: Maybe [CachedAttraction] <- Hedis.get redisKey
  case mbCachedAttractions of
    Just places | not (null places) -> do
      let finalPlaces = L.take requestedCount $ mkAttractions $ filterWithInRadius userPos requestedRadiusKm places
      pure $ API.AttractionRecommendResp {API.attractions = finalPlaces}
    _ -> do
      allRecommendedPlaces <- filter (\place -> isJust place.lat && isJust place.lon) <$> QTicketPlace.findAllRecommendationTicketPlaces True
      let attractionPlaces = catMaybes $ map mkCachedAttraction allRecommendedPlaces
          finalPlaces = L.take requestedCount $ mkAttractions $ filterWithInRadius userPos requestedRadiusKm attractionPlaces
      Hedis.setExp redisKey attractionPlaces 86400
      pure $ API.AttractionRecommendResp {API.attractions = finalPlaces}
  where
    getKm :: HighPrecMeters -> Double
    getKm m = (fromIntegral . (.getMeters) . highPrecMetersToMeters $ m) / 1000.0

    filterWithInRadius :: LatLong -> Double -> [CachedAttraction] -> [(CachedAttraction, Double)]
    filterWithInRadius userpos radiusInKm = filter (\(_, dist) -> dist <= radiusInKm) . map (\place -> (place, getKm $ distanceBetweenInMeters userpos (LatLong place.lat place.lon)))

    mkCachedAttraction :: DTicketPlace.TicketPlace -> Maybe CachedAttraction
    mkCachedAttraction place = if isJust place.lat && isJust place.lon then Just CachedAttraction {id = place.id, name = place.name, iconUrl = place.iconUrl, lat = fromMaybe 0 place.lat, lon = fromMaybe 0 place.lon} else Nothing

    mkAttractions :: [(CachedAttraction, Double)] -> [API.Attraction]
    mkAttractions = map (\(place, dist) -> API.Attraction {id = place.id, iconUrl = place.iconUrl, name = place.name, distanceInKm = dist})
