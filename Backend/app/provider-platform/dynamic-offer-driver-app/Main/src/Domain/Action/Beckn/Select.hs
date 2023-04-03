{-# LANGUAGE TypeApplications #-}
{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Domain.Action.Beckn.Select
  ( DSelectReq (..),
    handler,
  )
where

import Data.Time.Clock (addUTCTime)
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.FareParameters as DFareParams (FarePolicyType (..))
import qualified Domain.Types.FarePolicy as DFarePolicy (ExtraFee (..))
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchStep as DSS
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, logDebug, logInfo)
import Lib.Scheduler.JobStorageType.DB.Queries (createJobIn)
import Lib.Scheduler.Types (ExecutionResult (ReSchedule))
import SharedLogic.Allocator
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers (sendSearchRequestToDrivers')
import qualified SharedLogic.CacheDistance as CD
import SharedLogic.DriverPool (getDriverPoolConfig)
import SharedLogic.FareCalculator
import qualified Storage.CachedQueries.FarePolicy as FarePolicyS
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.CachedQueries.SlabFarePolicy as SFarePolicyS
import qualified Storage.Queries.Estimate as QEst
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchStep as QSS
import Tools.Error
import Tools.Maps as Maps

data DSelectReq = DSelectReq
  { messageId :: Text,
    transactionId :: Text,
    bapId :: Text,
    bapUri :: BaseUrl,
    pickupTime :: UTCTime,
    estimateId :: Id DEst.Estimate
  }

handler :: Id DM.Merchant -> DSelectReq -> Flow ()
handler merchantId sReq = do
  sessiontoken <- generateGUIDText
  merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  mbDistRes <- CD.getCacheDistance sReq.transactionId
  estimate <- QEst.findById sReq.estimateId >>= fromMaybeM (EstimateDoesNotExist sReq.estimateId.getId)
  searchReq <- QSR.findById estimate.requestId >>= fromMaybeM (SearchRequestNotFound estimate.requestId.getId)
  logInfo $ "Fetching cached distance and duration" <> show mbDistRes
  (distance, duration) <-
    case mbDistRes of
      Nothing -> do
        res <-
          Maps.getDistance merchantId $
            Maps.GetDistanceReq
              { origin = searchReq.fromLocation,
                destination = searchReq.toLocation,
                travelMode = Just Maps.CAR
              }
        pure (res.distance, res.duration)
      Just distRes -> pure distRes
  (fareParams, driverExtraFare) <- case merchant.farePolicyType of
    DFareParams.SLAB -> do
      slabFarePolicy <- SFarePolicyS.findByMerchantIdAndVariant merchant.id estimate.vehicleVariant >>= fromMaybeM (InternalError "Slab fare policy not found")
      let driverExtraFare = DFarePolicy.ExtraFee {minFee = 0, maxFee = 0}
      fareParams <- calculateFare merchantId (Right slabFarePolicy) distance sReq.pickupTime Nothing
      pure (fareParams, driverExtraFare)
    DFareParams.NORMAL -> do
      farePolicy <- FarePolicyS.findByMerchantIdAndVariant merchant.id estimate.vehicleVariant (Just distance) >>= fromMaybeM NoFarePolicy
      fareParams <- calculateFare merchantId (Left farePolicy) distance sReq.pickupTime Nothing
      pure (fareParams, farePolicy.driverExtraFee)
  searchStep <- buildSearchStep merchantId searchReq.id estimate sReq distance duration
  let estimateFare = fareSum fareParams
  logDebug $
    "search step id=" <> show searchStep.id
      <> "; estimated distance = "
      <> show distance
      <> "; estimated base fare:"
      <> show estimateFare
  driverPoolConfig <- getDriverPoolConfig merchantId distance
  let inTime = fromIntegral driverPoolConfig.singleBatchProcessTime
  Esq.runTransaction $ do
    QSS.create searchStep

  res <- sendSearchRequestToDrivers' driverPoolConfig searchReq searchStep merchant estimateFare driverExtraFare.minFee driverExtraFare.maxFee
  case res of
    ReSchedule _ -> do
      maxShards <- asks (.maxShards)
      Esq.runTransaction $ do
        createJobIn @_ @'SendSearchRequestToDriver inTime maxShards $
          SendSearchRequestToDriverJobData
            { searchStepId = searchStep.id,
              baseFare = estimateFare,
              estimatedRideDistance = distance,
              driverMinExtraFee = driverExtraFare.minFee,
              driverMaxExtraFee = driverExtraFare.maxFee
            }
    _ -> return ()

buildSearchStep ::
  ( MonadTime m,
    MonadGuid m,
    MonadReader r m,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime
  ) =>
  Id DM.Merchant ->
  Id DSR.SearchRequest ->
  DEst.Estimate ->
  DSelectReq ->
  Meters ->
  Seconds ->
  m DSS.SearchStep
buildSearchStep merchantId searchReqId estimate sReq distance duration = do
  now <- getCurrentTime
  id_ <- Id <$> generateGUID
  searchRequestExpirationSeconds <- asks (.searchRequestExpirationSeconds)
  let validTill_ = searchRequestExpirationSeconds `addUTCTime` now
  pure
    DSS.SearchStep
      { id = id_,
        requestId = searchReqId,
        estimateId = estimate.id,
        messageId = sReq.messageId,
        startTime = sReq.pickupTime,
        validTill = validTill_,
        vehicleVariant = estimate.vehicleVariant,
        status = DSS.ACTIVE,
        searchRepeatCounter = 0,
        createdAt = now,
        updatedAt = now
      }
