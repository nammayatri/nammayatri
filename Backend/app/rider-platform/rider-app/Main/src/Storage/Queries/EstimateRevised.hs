{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.EstimateRevised where

import Domain.Types.EstimateRevised as DER
import Domain.Types.SearchRequest
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id (Id (Id, getId))
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.EstimateRevised as BeamER
import qualified Storage.Queries.EstimateRevisedBreakup as QERB
import qualified Storage.Queries.TripTerms as QTT

createEstimateRevised :: MonadFlow m => EstimateRevised -> m ()
createEstimateRevised = createWithKV

create :: MonadFlow m => EstimateRevised -> m ()
create estimate = do
  logDebug $ "hello world create start"
  _ <- traverse_ QTT.createTripTerms estimate.tripTerms
  logDebug $ "hello world after tripterms"
  _ <- createEstimateRevised estimate
  logDebug $ "hello world before farebreakup"
  traverse_ QERB.create estimate.estimateRevisedBreakupList
  logDebug $ "hello world create end"

createMany :: MonadFlow m => [EstimateRevised] -> m ()
createMany = traverse_ create

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id EstimateRevised -> m (Maybe EstimateRevised)
findById (Id estimateId) = findOneWithKV [Se.Is BeamER.id $ Se.Eq estimateId]

findAllBySRId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id SearchRequest -> m [EstimateRevised]
findAllBySRId (Id searchRequestId) = findAllWithKV [Se.Is BeamER.requestId $ Se.Eq searchRequestId]

findByBPPEstimateId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id BPPEstimateRevised -> m (Maybe EstimateRevised)
findByBPPEstimateId (Id bppEstimateId_) = findOneWithKV [Se.Is BeamER.bppEstimateRevisedId $ Se.Eq bppEstimateId_]

updateStatus :: MonadFlow m => Id EstimateRevised -> EstimateStatus -> m ()
updateStatus (Id estimateId) status_ = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamER.updatedAt now,
      Se.Set BeamER.status status_
    ]
    [Se.Is BeamER.id (Se.Eq estimateId)]

getStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id EstimateRevised -> m (Maybe EstimateStatus)
getStatus (Id estimateId) = findOneWithKV [Se.Is BeamER.id $ Se.Eq estimateId] <&> (DER.status <$>)

updateStatusByRequestId :: MonadFlow m => Id SearchRequest -> EstimateStatus -> m ()
updateStatusByRequestId (Id searchId) status_ = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamER.updatedAt now,
      Se.Set BeamER.status status_
    ]
    [Se.Is BeamER.requestId (Se.Eq searchId)]

instance FromTType' BeamER.EstimateRevised EstimateRevised where
  fromTType' BeamER.EstimateRevisedT {..} = do
    etB <- QERB.findAllByEstimateRevisedIdT (Id id)
    trip <- if isJust tripTermsId then QTT.findById'' (Id (fromJust tripTermsId)) else pure Nothing
    pUrl <- parseBaseUrl providerUrl
    let totalFareRange =
          DER.FareRange
            { minFare = roundToIntegral minTotalFare,
              maxFare = roundToIntegral maxTotalFare
            }
    pure $
      Just $
        EstimateRevised
          { id = Id id,
            requestId = Id requestId,
            merchantId = Id <$> merchantId,
            merchantOperatingCityId = Id <$> merchantOperatingCityId,
            bppEstimateRevisedId = Id bppEstimateRevisedId,
            itemId = itemId,
            discount = roundToIntegral <$> discount,
            estimatedFare = roundToIntegral estimatedFare,
            estimatedTotalFare = roundToIntegral estimatedTotalFare,
            totalFareRange = totalFareRange,
            estimatedDuration = estimatedDuration,
            estimatedDistance = estimatedDistance,
            device = device,
            providerId = providerId,
            providerUrl = pUrl,
            providerName = providerName,
            providerMobileNumber = providerMobileNumber,
            providerCompletedRidesCount = providerCompletedRidesCount,
            vehicleVariant = vehicleVariant,
            tripTerms = trip,
            estimateRevisedBreakupList = etB,
            nightShiftInfo =
              ((,,) <$> nightShiftCharge <*> nightShiftStart <*> nightShiftEnd)
                <&> \(nightShiftCharge', nightShiftStart', nightShiftEnd') ->
                  DER.NightShiftInfo
                    { nightShiftCharge = nightShiftCharge',
                      oldNightShiftCharge = oldNightShiftCharge,
                      nightShiftStart = nightShiftStart',
                      nightShiftEnd = nightShiftEnd'
                    },
            status = status,
            waitingCharges = DER.WaitingCharges waitingChargePerMin,
            driversLocation = driversLocation,
            specialLocationTag = specialLocationTag,
            updatedAt = updatedAt,
            createdAt = createdAt
          }

instance ToTType' BeamER.EstimateRevised EstimateRevised where
  toTType' EstimateRevised {..} = do
    BeamER.EstimateRevisedT
      { BeamER.id = getId id,
        BeamER.requestId = getId requestId,
        BeamER.merchantId = getId <$> merchantId,
        BeamER.merchantOperatingCityId = getId <$> merchantOperatingCityId,
        BeamER.bppEstimateRevisedId = getId bppEstimateRevisedId,
        BeamER.itemId = itemId,
        BeamER.estimatedFare = realToFrac estimatedFare,
        BeamER.discount = realToFrac <$> discount,
        BeamER.estimatedTotalFare = realToFrac estimatedTotalFare,
        BeamER.minTotalFare = realToFrac totalFareRange.minFare,
        BeamER.maxTotalFare = realToFrac totalFareRange.maxFare,
        BeamER.estimatedDuration = estimatedDuration,
        BeamER.estimatedDistance = estimatedDistance,
        BeamER.device = device,
        BeamER.providerId = providerId,
        BeamER.providerUrl = showBaseUrl providerUrl,
        BeamER.providerName = providerName,
        BeamER.providerMobileNumber = providerMobileNumber,
        BeamER.providerCompletedRidesCount = providerCompletedRidesCount,
        BeamER.vehicleVariant = vehicleVariant,
        BeamER.driversLocation = driversLocation,
        BeamER.tripTermsId = getId <$> (tripTerms <&> (.id)),
        BeamER.nightShiftCharge = nightShiftInfo <&> (.nightShiftCharge),
        BeamER.oldNightShiftCharge = (.oldNightShiftCharge) =<< nightShiftInfo,
        BeamER.nightShiftStart = nightShiftInfo <&> (.nightShiftStart),
        BeamER.nightShiftEnd = nightShiftInfo <&> (.nightShiftEnd),
        BeamER.status = status,
        BeamER.waitingChargePerMin = waitingCharges.waitingChargePerMin,
        BeamER.specialLocationTag = specialLocationTag,
        BeamER.updatedAt = updatedAt,
        BeamER.createdAt = createdAt
      }
