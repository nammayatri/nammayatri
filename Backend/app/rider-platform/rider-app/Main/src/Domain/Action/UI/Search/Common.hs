{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Search.Common
  ( SearchReqLocation (..),
    buildSearchReqLoc,
    buildSearchRequest,
    makeSearchReqLoc',
  )
where

import qualified Domain.Types.Location as Location
import Domain.Types.LocationAddress
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SearchRequest as SearchRequest
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Types.Version
import Kernel.Utils.Common
import Tools.Metrics (CoreMetrics)

buildSearchRequest ::
  ( (HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds]),
    EsqDBFlow m r,
    CoreMetrics m,
    MonadFlow m
  ) =>
  DPerson.Person ->
  Location.Location ->
  DMOC.MerchantOperatingCity ->
  Maybe Location.Location ->
  Maybe HighPrecMeters ->
  Maybe HighPrecMeters ->
  UTCTime ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  Maybe Text ->
  Maybe Seconds ->
  m SearchRequest.SearchRequest
buildSearchRequest person pickup merchantOperatingCity mbDrop mbMaxDistance mbDistance now bundleVersion clientVersion device disabilityTag duration = do
  searchRequestId <- generateGUID
  validTill <- getSearchRequestExpiry now
  return
    SearchRequest.SearchRequest
      { id = searchRequestId,
        startTime = now,
        validTill = validTill,
        riderId = person.id,
        fromLocation = pickup,
        toLocation = mbDrop,
        distance = mbDistance,
        maxDistance = mbMaxDistance,
        merchantId = person.merchantId,
        merchantOperatingCityId = merchantOperatingCity.id,
        createdAt = now,
        estimatedRideDuration = duration,
        device = device,
        bundleVersion = bundleVersion,
        clientVersion = clientVersion,
        language = person.language,
        disabilityTag = disabilityTag,
        customerExtraFee = Nothing,
        autoAssignEnabled = Nothing,
        autoAssignEnabledV2 = Nothing,
        availablePaymentMethods = [],
        selectedPaymentMethodId = Nothing
      }
  where
    getSearchRequestExpiry :: (HasFlowEnv m r '["searchRequestExpiry" ::: Maybe Seconds]) => UTCTime -> m UTCTime
    getSearchRequestExpiry _ = do
      searchRequestExpiry <- maybe 1800 fromIntegral <$> asks (.searchRequestExpiry)
      pure $ addUTCTime (fromInteger searchRequestExpiry) now

data SearchReqLocation = SearchReqLocation
  { gps :: LatLong,
    address :: LocationAddress
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

buildSearchReqLoc :: MonadFlow m => SearchReqLocation -> m Location.Location
buildSearchReqLoc SearchReqLocation {..} = do
  now <- getCurrentTime
  locId <- generateGUID
  return
    Location.Location
      { id = locId,
        lat = gps.lat,
        lon = gps.lon,
        address = address,
        createdAt = now,
        updatedAt = now
      }

makeSearchReqLoc' :: Location.Location -> SearchReqLocation
makeSearchReqLoc' Location.Location {..} =
  SearchReqLocation
    { gps = LatLong lat lon,
      ..
    }
