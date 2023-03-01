{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Domain.Action.Beckn.Select where

import qualified Beckn.Types.Core.Taxi.Common.Address as BA
import qualified Data.Map as M
import Data.Time.Clock (addUTCTime)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import Domain.Types.Vehicle.Variant (Variant)
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, logDebug, logInfo)
import Lib.Scheduler.Types (ExecutionResult (ReSchedule))
import SharedLogic.Allocator
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers (sendSearchRequestToDrivers')
import qualified SharedLogic.CacheDistance as CD
import SharedLogic.DriverPool (getDriverPoolConfig)
import SharedLogic.FareCalculator
import SharedLogic.GoogleMaps
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.FarePolicy.FarePolicy as FarePolicyS
import qualified Storage.CachedQueries.Merchant as QMerch
import Storage.Queries.AllocatorJob (createAllocatorSendSearchRequestToDriverJob)
import qualified Storage.Queries.SearchRequest as QSReq
import Tools.Error (FarePolicyError (NoFarePolicy), MerchantError (MerchantNotFound))
import Tools.Maps as Maps

data DSelectReq = DSelectReq
  { messageId :: Text,
    transactionId :: Text,
    bapId :: Text,
    bapUri :: BaseUrl,
    pickupLocation :: LatLong,
    pickupTime :: UTCTime,
    dropLocation :: LatLong,
    pickupAddress :: Maybe BA.Address,
    dropAddrress :: Maybe BA.Address,
    variant :: Variant,
    autoAssignEnabled :: Bool,
    customerExtraFee :: Maybe Money
  }

type LanguageDictionary = M.Map Maps.Language DSearchReq.SearchRequest

handler :: Id DM.Merchant -> DSelectReq -> Flow ()
handler merchantId sReq = do
  sessiontoken <- generateGUIDText
  fromLocation <- buildSearchReqLocation merchantId sessiontoken sReq.pickupAddress sReq.pickupLocation
  toLocation <- buildSearchReqLocation merchantId sessiontoken sReq.dropAddrress sReq.dropLocation
  mbDistRes <- CD.getCacheDistance sReq.transactionId
  logInfo $ "Fetching cached distance and duration" <> show mbDistRes
  (distance, duration) <-
    case mbDistRes of
      Nothing -> do
        res <-
          Maps.getDistance merchantId $
            Maps.GetDistanceReq
              { origin = fromLocation,
                destination = toLocation,
                travelMode = Just Maps.CAR
              }
        pure (res.distance, res.duration)
      Just distRes -> pure distRes
  farePolicy <- FarePolicyS.findByMerchantIdAndVariant merchantId sReq.variant (Just distance) >>= fromMaybeM NoFarePolicy
  fareParams <- calculateFare merchantId farePolicy distance sReq.pickupTime Nothing sReq.customerExtraFee
  searchReq <- buildSearchRequest fromLocation toLocation merchantId sReq distance duration
  let driverExtraFare = farePolicy.driverExtraFee
  let baseFare = fareSum fareParams
  logDebug $
    "search request id=" <> show searchReq.id
      <> "; estimated distance = "
      <> show distance
      <> "; estimated base fare:"
      <> show baseFare
  merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)

  inTime <- fromIntegral <$> asks (.sendSearchRequestJobCfg.singleBatchProcessTime)
  Esq.runTransaction $ do
    QSReq.create searchReq

  driverPoolConfig <- getDriverPoolConfig distance
  res <- sendSearchRequestToDrivers' driverPoolConfig searchReq merchant baseFare sReq.customerExtraFee driverExtraFare.minFee driverExtraFare.maxFee
  case res of
    ReSchedule ut ->
      Esq.runTransaction $ do
        createAllocatorSendSearchRequestToDriverJob inTime $
          SendSearchRequestToDriverJobData
            { requestId = searchReq.id,
              baseFare = baseFare,
              estimatedRideDistance = distance,
              customerExtraFee = sReq.customerExtraFee,
              driverMinExtraFee = driverExtraFare.minFee,
              driverMaxExtraFee = driverExtraFare.maxFee
            }
    _ -> return ()

buildSearchRequest ::
  ( MonadTime m,
    MonadGuid m,
    MonadReader r m,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime
  ) =>
  DLoc.SearchReqLocation ->
  DLoc.SearchReqLocation ->
  Id DM.Merchant ->
  DSelectReq ->
  Meters ->
  Seconds ->
  m DSearchReq.SearchRequest
buildSearchRequest from to merchantId sReq distance duration = do
  now <- getCurrentTime
  id_ <- Id <$> generateGUID
  searchRequestExpirationSeconds <- asks (.searchRequestExpirationSeconds)
  let validTill_ = searchRequestExpirationSeconds `addUTCTime` now
  pure
    DSearchReq.SearchRequest
      { id = id_,
        transactionId = sReq.transactionId,
        messageId = sReq.messageId,
        startTime = sReq.pickupTime,
        validTill = validTill_,
        providerId = merchantId,
        fromLocation = from,
        toLocation = to,
        bapId = sReq.bapId,
        bapUri = sReq.bapUri,
        estimatedDistance = distance,
        estimatedDuration = duration,
        createdAt = now,
        vehicleVariant = sReq.variant,
        status = DSearchReq.ACTIVE,
        updatedAt = now,
        autoAssignEnabled = sReq.autoAssignEnabled
      }

buildSearchReqLocation :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => Id DM.Merchant -> Text -> Maybe BA.Address -> LatLong -> m DLoc.SearchReqLocation
buildSearchReqLocation merchantId sessionToken _ latLong@Maps.LatLong {..} = do
  pickupRes <-
    Maps.getPlaceName merchantId $
      Maps.GetPlaceNameReq
        { getBy = Maps.ByLatLong latLong,
          sessionToken = Just sessionToken,
          language = Nothing
        }
  Address {..} <- mkLocation pickupRes
  id <- Id <$> generateGUID
  now <- getCurrentTime
  let createdAt = now
      updatedAt = now
  pure DLoc.SearchReqLocation {..}
