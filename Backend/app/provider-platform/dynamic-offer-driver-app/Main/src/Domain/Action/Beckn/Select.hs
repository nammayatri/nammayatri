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
    validateRequest,
    handler,
  )
where

import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.FarePolicy as DFarePolicy
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (addUTCTime, fromMaybeM, logDebug)
import Lib.Scheduler.JobStorageType.DB.Queries (createJobIn)
import Lib.Scheduler.Types (ExecutionResult (ReSchedule))
import SharedLogic.Allocator
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers (sendSearchRequestToDrivers')
import SharedLogic.DriverPool (getDriverPoolConfig)
import SharedLogic.FareCalculator
import qualified Storage.CachedQueries.FarePolicy as FarePolicyS
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.Queries.Estimate as QEst
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchTry as QST
import Tools.Error

data DSelectReq = DSelectReq
  { messageId :: Text,
    transactionId :: Text,
    estimateId :: Id DEst.Estimate,
    bapId :: Text,
    bapUri :: BaseUrl,
    pickupTime :: UTCTime,
    estimateId :: Id DEst.Estimate,
    customerExtraFee :: Maybe Money
  }

handler :: DM.Merchant -> DSelectReq -> DEst.Estimate -> Flow ()
handler merchant sReq estimate = do
  let merchantId = merchant.id
  searchReq <- QSR.findById estimate.requestId >>= fromMaybeM (SearchRequestNotFound estimate.requestId.getId)

  farePolicy <- FarePolicyS.findByMerchantIdAndVariant merchantId estimate.vehicleVariant >>= fromMaybeM NoFarePolicy
  fareParams <-
    calculateFareParameters
      CalculateFareParametersParams
        { farePolicy = farePolicy,
          distance = searchReq.estimatedDistance,
          rideTime = sReq.pickupTime,
          waitingTime = Nothing,
          driverSelectedFare = Nothing,
          customerExtraFee = sReq.customerExtraFee
        }
  searchTry <- buildSearchTry merchantId searchReq.id estimate sReq.customerExtraFee sReq searchReq.estimatedDistance searchReq.estimatedDuration
  let estimateFare = fareSum fareParams
  logDebug $
    "search try id=" <> show searchTry.id
      <> "; estimated distance = "
      <> show searchReq.estimatedDistance
      <> "; estimated base fare:"
      <> show estimateFare
  driverPoolConfig <- getDriverPoolConfig merchantId searchReq.estimatedDistance
  let inTime = fromIntegral driverPoolConfig.singleBatchProcessTime
  Esq.runTransaction $ do
    QST.create searchTry

  let driverExtraFeeBounds = DFarePolicy.findDriverExtraFeeBoundsByDistance searchReq.estimatedDistance <$> farePolicy.driverExtraFeeBounds
  res <- sendSearchRequestToDrivers' driverPoolConfig searchReq searchTry merchant estimateFare driverExtraFeeBounds
  case res of
    ReSchedule _ -> do
      maxShards <- asks (.maxShards)
      Esq.runTransaction $ do
        createJobIn @_ @'SendSearchRequestToDriver inTime maxShards $
          SendSearchRequestToDriverJobData
            { searchTryId = searchTry.id,
              baseFare = estimateFare,
              estimatedRideDistance = searchReq.estimatedDistance,
              customerExtraFee = sReq.customerExtraFee,
              driverExtraFeeBounds = driverExtraFeeBounds
            }
    _ -> return ()

buildSearchTry ::
  ( MonadTime m,
    MonadGuid m,
    MonadReader r m,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime
  ) =>
  Id DM.Merchant ->
  Id DSR.SearchRequest ->
  DEst.Estimate ->
  Maybe Money ->
  DSelectReq ->
  Meters ->
  Seconds ->
  m DST.SearchTry
buildSearchTry merchantId searchReqId estimate customerExtraFee sReq distance duration = do
  now <- getCurrentTime
  id_ <- Id <$> generateGUID
  searchRequestExpirationSeconds <- asks (.searchRequestExpirationSeconds)
  let validTill_ = searchRequestExpirationSeconds `addUTCTime` now
  pure
    DST.SearchTry
      { id = id_,
        requestId = searchReqId,
        estimateId = estimate.id,
        messageId = sReq.messageId,
        startTime = sReq.pickupTime,
        validTill = validTill_,
        vehicleVariant = estimate.vehicleVariant,
        status = DST.ACTIVE,
        searchRepeatCounter = 0,
        createdAt = now,
        updatedAt = now,
        ..
      }

validateRequest :: Id DM.Merchant -> DSelectReq -> Flow (DM.Merchant, DEst.Estimate)
validateRequest merchantId sReq = do
  merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  estimate <- QEst.findById sReq.estimateId >>= fromMaybeM (EstimateDoesNotExist sReq.estimateId.getId)
  return (merchant, estimate)
