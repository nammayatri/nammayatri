{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.OnSearch.OneWay where

import qualified Data.Text as T
import Data.Traversable
import qualified Domain.Types.BusinessEvent as SB
import qualified Domain.Types.FarePolicy.FareProduct as SFP
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.SearchRequest as DSearchRequest
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import qualified Domain.Types.Vehicle as DVeh
import EulerHS.Prelude hiding (id, state)
import qualified Kernel.External.Maps.Types as MapSearch
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CacheDistance as CD
import qualified SharedLogic.DriverPool as DrPool
import SharedLogic.FareCalculator.OneWayFareCalculator
import qualified SharedLogic.FareCalculator.OneWayFareCalculator.Flow as Fare
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.BusinessEvent as QBE
import qualified Storage.Queries.Quote as QQuote
import Tools.Maps (HasCoordinates (..))
import qualified Tools.Maps as MapSearch
import Tools.Metrics (CoreMetrics, HasBPPMetrics)

data QuoteInfo = QuoteInfo
  { quoteId :: Id DQuote.Quote,
    vehicleVariant :: DVeh.Variant,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    distanceToNearestDriver :: Meters,
    fromLocation :: MapSearch.LatLong,
    toLocation :: MapSearch.LatLong,
    startTime :: UTCTime
  }

onSearchCallback ::
  ( HasCacheConfig r,
    EncFlow m r,
    EsqDBFlow m r,
    HedisFlow m r,
    DrPool.HasDriverPoolConfig r,
    HasFlowEnv m r '["driverEstimatedPickupDuration" ::: Seconds],
    HasBPPMetrics m r,
    CoreMetrics m
  ) =>
  DSearchRequest.SearchRequest ->
  Id DM.Merchant ->
  UTCTime ->
  DLoc.SearchReqLocation ->
  DLoc.SearchReqLocation ->
  Text ->
  m [QuoteInfo]
onSearchCallback searchRequest transporterId now fromLocation toLocation transactionId = do
  pool <- DrPool.calculateDriverPool fromLocation transporterId Nothing SFP.ONE_WAY Nothing
  logTagInfo "OnSearchCallback" $
    "Calculated Driver Pool for organization " +|| getId transporterId
      ||+ " with drivers " +| T.intercalate ", " (getId . (.driverId) <$> pool) |+ ""
  Esq.runTransaction $ traverse_ (QBE.logDriverInPoolEvent SB.ON_SEARCH Nothing) pool
  let listOfProtoQuotes =
        catMaybes $
          everyPossibleVariant <&> \var ->
            find ((== var) . (.variant)) pool
  -- drivers sorted from nearest to furthest, so with `find`
  -- we take nearest one and calculate fare and make PI for him

  driverEstimatedPickupDuration <- asks (.driverEstimatedPickupDuration)
  distRes <-
    MapSearch.getDistance
      transporterId
      MapSearch.GetDistanceReq
        { origin = fromLocation,
          destination = toLocation,
          travelMode = Just MapSearch.CAR
        }
      (dataDecider 300)
  let distance = distRes.distance
      estimatedRideDuration = distRes.duration
      estimatedRideFinishTime = realToFrac (driverEstimatedPickupDuration + estimatedRideDuration) `addUTCTime` searchRequest.startTime
  CD.cacheDistance transactionId (distance, estimatedRideDuration)
  listOfQuotes <-
    for listOfProtoQuotes $ \poolResult -> do
      fareParams <- calculateFare transporterId poolResult.variant distance estimatedRideFinishTime
      buildOneWayQuote
        searchRequest
        fareParams
        transporterId
        distance
        poolResult.distanceToPickup
        poolResult.variant
        estimatedRideFinishTime
        now
  Esq.runTransaction $
    for_ listOfQuotes QQuote.create
  pure $ mkQuoteInfo fromLocation toLocation now <$> listOfQuotes

dataDecider :: Seconds -> NonEmpty (Meters, Seconds) -> (Meters, Seconds)
dataDecider threshold (x :| xs) = foldl' decider x xs
  where
    decider (distance1, duration1) (distance2, duration2)
      | distance1 > distance2 =
        if duration1 < duration2 && duration2 - duration1 < threshold
          then -- first longer but faster within threshold
            (distance1, duration1)
          else -- second shorter and faster or threshold passed
            (distance2, duration2)
      | distance1 < distance2 =
        if duration1 > duration2 && duration1 - duration2 < threshold
          then -- second longer but faster within threshold
            (distance2, duration2)
          else -- first shorter and faster or threshold passed
            (distance1, duration1)
      | otherwise = (distance1, min duration1 duration2)

buildOneWayQuote ::
  EsqDBFlow m r =>
  DSearchRequest.SearchRequest ->
  Fare.OneWayFareParameters ->
  Id DM.Merchant ->
  Meters ->
  Meters ->
  DVeh.Variant ->
  UTCTime ->
  UTCTime ->
  m DQuote.Quote
buildOneWayQuote productSearchRequest fareParams transporterId distance distanceToNearestDriver vehicleVariant estimatedFinishTime now = do
  quoteId <- Id <$> generateGUID
  let estimatedFare = fareSum fareParams
      discount = fareParams.discount
      estimatedTotalFare = fareSumWithDiscount fareParams
  let oneWayQuoteDetails = DQuote.OneWayQuoteDetails {..}
  pure
    DQuote.Quote
      { id = quoteId,
        requestId = productSearchRequest.id,
        providerId = transporterId,
        createdAt = now,
        quoteDetails = DQuote.OneWayDetails oneWayQuoteDetails,
        ..
      }

mkQuoteInfo :: DLoc.SearchReqLocation -> DLoc.SearchReqLocation -> UTCTime -> DQuote.Quote -> QuoteInfo
mkQuoteInfo fromLoc toLoc startTime DQuote.Quote {..} = do
  let fromLocation = getCoordinates fromLoc
      toLocation = getCoordinates toLoc
      distanceToNearestDriver = case quoteDetails of
        DQuote.OneWayDetails details -> DQuote.distanceToNearestDriver details
        _ -> 0
  QuoteInfo
    { quoteId = id,
      ..
    }
