{-# OPTIONS_GHC -Wno-unused-matches #-}

module Domain.Action.Beckn.Select where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Tools.Metrics.CoreMetrics
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Common (fromMaybeM, logDebug, logInfo)
import qualified Data.Map as M
import Data.Time.Clock (addUTCTime)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import Domain.Types.Vehicle.Variant (Variant)
import Environment
import Lib.Scheduler.Types (ExecutionResult (ReSchedule))
import SharedLogic.Allocator
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers (sendSearchRequestToDrivers')
import qualified SharedLogic.CacheDistance as CD
import SharedLogic.DriverPool (getDriverPoolConfig)
import SharedLogic.FareCalculator
import SharedLogic.GoogleMaps
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.FarePolicy as FarePolicyS
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
    variant :: Variant
  }

type LanguageDictionary = M.Map Maps.Language DSearchReq.SearchRequest

handler :: Id DM.Merchant -> DSelectReq -> Flow ()
handler merchantId sReq = do
  sessiontoken <- generateGUIDText
  fromLocation <- buildSearchReqLocation merchantId sessiontoken sReq.pickupLocation
  toLocation <- buildSearchReqLocation merchantId sessiontoken sReq.dropLocation
  farePolicy <- FarePolicyS.findByMerchantIdAndVariant merchantId sReq.variant >>= fromMaybeM NoFarePolicy
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
  fareParams <- calculateFare merchantId farePolicy distance sReq.pickupTime Nothing
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
  res <- sendSearchRequestToDrivers' driverPoolConfig searchReq merchant baseFare driverExtraFare.minFee driverExtraFare.maxFee
  case res of
    ReSchedule ut ->
      Esq.runTransaction $ do
        createAllocatorSendSearchRequestToDriverJob inTime $
          SendSearchRequestToDriverJobData
            { requestId = searchReq.id,
              baseFare = baseFare,
              estimatedRideDistance = distance,
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
        vehicleVariant = sReq.variant
      }

buildSearchReqLocation :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => Id DM.Merchant -> Text -> LatLong -> m DLoc.SearchReqLocation
buildSearchReqLocation merchantId sessionToken latLong@Maps.LatLong {..} = do
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
