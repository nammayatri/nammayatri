{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Domain.Action.UI.Search.Recurring
  ( RecurringSearchReq (..),
    RecurringSearchRes (..),
    DSearch.SearchReqLocation (..),
    recurringSearch,
  )
where

import Data.OpenApi (NamedSchema (..))
import Data.OpenApi.Schema
import qualified Data.Set as S
import Data.Time.Calendar (DayOfWeek)
import qualified Domain.Action.UI.Search.Common as DSearch
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import qualified Domain.Types.SearchRequest as DSearchReq
import Kernel.Prelude
import Kernel.Serviceability
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Types.Version (Version)
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as QMerchant
import Storage.Queries.Geometry
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Error
import qualified Tools.Maps as MapSearch
import Tools.Metrics
import qualified Tools.Metrics as Metrics

newtype Schedule = Schedule {unwrap :: S.Set DayOfWeek}
  deriving stock (Generic)
  deriving newtype (FromJSON, ToJSON, Show)

instance ToSchema Schedule where
  declareNamedSchema _ = do
    schedule <- declareSchema (Proxy :: Proxy (S.Set Text))
    return $
      NamedSchema (Just "Schedule") $ schedule

data RecurringSearchReq = RecurringSearchReq
  { origin :: DSearch.SearchReqLocation,
    destination :: DSearch.SearchReqLocation,
    initialRideTime :: UTCTime,
    scheduleDays :: Schedule
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data RecurringSearchRes = RecurringSearchRes
  { origin :: DSearch.SearchReqLocation,
    destination :: DSearch.SearchReqLocation,
    searchId :: Id DSearchReq.SearchRequest,
    now :: UTCTime,
    initialRideTime :: UTCTime,
    scheduleDays :: S.Set DayOfWeek,
    --TODO: This supposed to be temporary solution. Check if we still need it
    gatewayUrl :: BaseUrl,
    searchRequestExpiry :: UTCTime
  }

recurringSearch ::
  ( HasCacheConfig r,
    EncFlow m r,
    HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds],
    HedisFlow m r,
    EsqDBFlow m r,
    HedisFlow m r,
    CoreMetrics m,
    HasBAPMetrics m r
  ) =>
  Id Person.Person ->
  RecurringSearchReq ->
  Maybe Version ->
  Maybe Version ->
  m RecurringSearchRes
recurringSearch personId req bundleVersion clientVersion = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  merchant <- QMerchant.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
  validateServiceability merchant.geofencingConfig
  fromLocation <- DSearch.buildSearchReqLoc req.origin
  toLocation <- DSearch.buildSearchReqLoc req.destination
  now <- getCurrentTime
  distance <-
    (\res -> metersToHighPrecMeters res.distance)
      <$> MapSearch.getDistance merchant.id
        MapSearch.GetDistanceReq
          { origin = req.origin.gps,
            destination = req.destination.gps,
            travelMode = Just MapSearch.CAR
          }
  searchRequest <- DSearch.buildSearchRequest person fromLocation (Just toLocation) (Just distance) req.initialRideTime bundleVersion clientVersion
  Metrics.incrementSearchRequestCount merchant.name
  let txnId = getId (searchRequest.id)
  Metrics.startSearchMetrics merchant.name txnId
  DB.runTransaction $ do
    QSearchRequest.create searchRequest
    QPFS.updateStatus personId DPFS.SEARCHING {requestId = searchRequest.id, validTill = searchRequest.validTill}
  let dSearchRes =
        RecurringSearchRes
          { origin = req.origin,
            destination = req.destination,
            searchId = searchRequest.id,
            now = now,
            initialRideTime = req.initialRideTime,
            scheduleDays = req.scheduleDays.unwrap,
            gatewayUrl = merchant.gatewayUrl,
            searchRequestExpiry = searchRequest.validTill
          }
  return dSearchRes
  where
    validateServiceability geoConfig =
      unlessM (rideServiceable geoConfig someGeometriesContain req.origin.gps (Just req.destination.gps)) $
        throwError RideNotServiceable
