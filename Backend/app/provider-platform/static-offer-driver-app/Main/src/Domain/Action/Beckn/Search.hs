{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.Search
  ( DSearchReq (..),
    DSearchRes (..),
    LocationReq (..),
    search,
  )
where

import Data.Traversable
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import Environment
import EulerHS.Prelude hiding (id, state)
import Kernel.External.Maps.HasCoordinates
import Kernel.Prelude (ToSchema)
import Kernel.Serviceability
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Geometry as QGeometry
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Error
import qualified Tools.Metrics as Metrics

data DSearchReq = DSearchReq
  { messageId :: Text,
    transactionId :: Text,
    bapId :: Text,
    bapUri :: BaseUrl,
    pickupLocation :: LocationReq,
    pickupTime :: UTCTime,
    mbDropLocation :: Maybe LocationReq
  }

data DSearchRes = DSearchRes
  { searchRequest :: DSR.SearchRequest,
    transporter :: DM.Merchant,
    fromLocation :: DLoc.SearchReqLocation,
    mbToLocation :: Maybe DLoc.SearchReqLocation,
    searchMetricsMVar :: Metrics.SearchMetricsMVar
  }

data LocationReq = LocationReq
  { lat :: Double,
    lon :: Double
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema, HasCoordinates)

search :: Id DM.Merchant -> DSearchReq -> Flow DSearchRes
search transporterId req@DSearchReq {..} = do
  transporter <- QM.findById transporterId >>= fromMaybeM (MerchantDoesNotExist transporterId.getId)
  unless transporter.enabled $ throwError AgencyDisabled
  let pickupLatLong = getCoordinates pickupLocation
  let mbDropoffLatLong = getCoordinates <$> mbDropLocation
  unlessM (rideServiceable transporter.geofencingConfig QGeometry.someGeometriesContain pickupLatLong mbDropoffLatLong) $
    throwError RideNotServiceable
  whenJustM
    (QSearchRequest.findByMsgIdAndBapIdAndBppId messageId bapId transporter.id)
    (\_ -> throwError $ InvalidRequest "Duplicate Search request")

  searchMetricsMVar <- Metrics.startSearchMetrics transporter.name

  now <- getCurrentTime
  validity <- getValidTime now pickupTime
  fromLocation <- buildSearchReqLoc now pickupLocation
  mbToLocation <- buildSearchReqLoc now `traverse` mbDropLocation
  searchRequest <- buildSearchRequest req transporter.id now validity fromLocation mbToLocation
  Esq.runTransaction $ do
    --These things are used only for analitics
    QSearchRequest.create searchRequest
  pure DSearchRes {..}

buildSearchReqLoc ::
  MonadGuid m =>
  UTCTime ->
  LocationReq ->
  m DLoc.SearchReqLocation
buildSearchReqLoc now LocationReq {..} = do
  locId <- generateGUID
  return
    DLoc.SearchReqLocation
      { id = locId,
        createdAt = now,
        updatedAt = now,
        ..
      }

getValidTime :: HasFlowEnv m r '["searchExpiry" ::: Maybe Seconds] => UTCTime -> UTCTime -> m UTCTime
getValidTime now startTime = do
  caseExpiry_ <- maybe 7200 fromIntegral <$> asks (.searchExpiry)
  let minExpiry = 300 -- 5 minutes
      timeToRide = startTime `diffUTCTime` now
      validTill = addUTCTime (minimum [fromInteger caseExpiry_, maximum [minExpiry, timeToRide]]) now
  pure validTill

buildSearchRequest ::
  MonadGuid m =>
  DSearchReq ->
  Id DM.Merchant ->
  UTCTime ->
  UTCTime ->
  DLoc.SearchReqLocation ->
  Maybe DLoc.SearchReqLocation ->
  m DSR.SearchRequest
buildSearchRequest DSearchReq {..} transporterId now validity fromLocation mbToLocation = do
  uuid <- generateGUID
  pure
    DSR.SearchRequest
      { id = Id uuid,
        transactionId = transactionId,
        messageId = messageId,
        startTime = pickupTime,
        validTill = validity,
        providerId = transporterId,
        fromLocation = fromLocation,
        toLocation = mbToLocation,
        bapId = bapId,
        bapUri = bapUri,
        createdAt = now
      }
