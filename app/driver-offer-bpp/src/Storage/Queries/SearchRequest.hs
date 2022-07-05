module Storage.Queries.SearchRequest where

import Beckn.Prelude
import Beckn.Product.MapSearch.GoogleMaps
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Types.MapSearch
import Beckn.Utils.GenericPretty (PrettyShow)
import Domain.Types.Organization
import Domain.Types.SearchReqLocation
import Domain.Types.SearchRequest
import Storage.Tabular.SearchReqLocation
import Storage.Tabular.SearchRequest

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 func (a, b, c) = func a b c

create :: SearchRequest -> SqlDB ()
create = Esq.create

findById :: Transactionable m => Id SearchRequest -> m (Maybe SearchRequest)
findById = Esq.findById

data NearbySearchRequestEntry = NearbySearchRequestEntry
  { searchRequest :: SearchRequest,
    fromLocation :: SearchReqLocation,
    straightDistanceMeters :: Double
  }
  deriving (Generic, PrettyShow, Show)

instance HasCoordinates NearbySearchRequestEntry where
  getCoordinates = getCoordinates . (.fromLocation)

findNearestByPoint :: (Transactionable m, Functor m) => Int -> Id Organization -> LatLong -> m [NearbySearchRequestEntry]
findNearestByPoint radiusMeters orgId driverPos =
  fmap (map $ uncurry3 NearbySearchRequestEntry) $
    Esq.findAll $ do
      sReq :& sFromLoc <- do
        from
          ( table @SearchRequestT
              `innerJoin` table @SearchReqLocationT `Esq.on` (\(s :& loc1) -> s ^. SearchRequestFromLocationId ==. loc1 ^. SearchReqLocationTId)
          )
      let dist = Esq.getPoint (val driverPos.lat, val driverPos.lon) <->. (sFromLoc ^. SearchReqLocationPoint)
      where_ $
        dist <=. val (fromIntegral radiusMeters)
          &&. sReq ^. SearchRequestProviderId ==. val (toKey orgId)
      pure (sReq, sFromLoc, dist)
