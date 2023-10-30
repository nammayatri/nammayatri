{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Estimate where

import Domain.Types.Estimate as DE
import Domain.Types.SearchRequest
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id (Id (Id, getId))
import qualified Sequelize as Se
import qualified Storage.Beam.Estimate as BeamE
import qualified Storage.Queries.EstimateBreakup as QEB
import qualified Storage.Queries.TripTerms as QTT

createEstimate :: MonadFlow m => Estimate -> m ()
createEstimate = createWithKV

create :: MonadFlow m => Estimate -> m ()
create estimate = do
  _ <- traverse_ QTT.createTripTerms estimate.tripTerms
  _ <- createEstimate estimate
  traverse_ QEB.create estimate.estimateBreakupList

createMany :: MonadFlow m => [Estimate] -> m ()
createMany = traverse_ create

findById :: MonadFlow m => Id Estimate -> m (Maybe Estimate)
findById (Id estimateId) = findOneWithKV [Se.Is BeamE.id $ Se.Eq estimateId]

findAllBySRId :: MonadFlow m => Id SearchRequest -> m [Estimate]
findAllBySRId (Id searchRequestId) = findAllWithKV [Se.Is BeamE.requestId $ Se.Eq searchRequestId]

findByBPPEstimateId :: MonadFlow m => Id BPPEstimate -> m (Maybe Estimate)
findByBPPEstimateId (Id bppEstimateId_) = findOneWithKV [Se.Is BeamE.bppEstimateId $ Se.Eq bppEstimateId_]

updateStatus :: MonadFlow m => Id Estimate -> EstimateStatus -> m ()
updateStatus (Id estimateId) status_ = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamE.updatedAt now,
      Se.Set BeamE.status status_
    ]
    [Se.Is BeamE.id (Se.Eq estimateId)]

getStatus :: MonadFlow m => Id Estimate -> m (Maybe EstimateStatus)
getStatus (Id estimateId) = findOneWithKV [Se.Is BeamE.id $ Se.Eq estimateId] <&> (DE.status <$>)

updateStatusByRequestId :: MonadFlow m => Id SearchRequest -> EstimateStatus -> m ()
updateStatusByRequestId (Id searchId) status_ = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamE.updatedAt now,
      Se.Set BeamE.status status_
    ]
    [Se.Is BeamE.requestId (Se.Eq searchId)]

instance FromTType' BeamE.Estimate Estimate where
  fromTType' BeamE.EstimateT {..} = do
    etB <- QEB.findAllByEstimateIdT (Id id)
    trip <- if isJust tripTermsId then QTT.findById'' (Id (fromJust tripTermsId)) else pure Nothing
    pUrl <- parseBaseUrl providerUrl
    let totalFareRange =
          DE.FareRange
            { minFare = roundToIntegral minTotalFare,
              maxFare = roundToIntegral maxTotalFare
            }
    pure $
      Just $
        Estimate
          { id = Id id,
            requestId = Id requestId,
            merchantId = Id <$> merchantId,
            bppEstimateId = Id bppEstimateId,
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
            estimateBreakupList = etB,
            nightShiftInfo =
              ((,,) <$> nightShiftCharge <*> nightShiftStart <*> nightShiftEnd)
                <&> \(nightShiftCharge', nightShiftStart', nightShiftEnd') ->
                  DE.NightShiftInfo
                    { nightShiftCharge = nightShiftCharge',
                      oldNightShiftCharge = oldNightShiftCharge,
                      nightShiftStart = nightShiftStart',
                      nightShiftEnd = nightShiftEnd'
                    },
            status = status,
            waitingCharges = DE.WaitingCharges waitingChargePerMin,
            driversLocation = driversLocation,
            specialLocationTag = specialLocationTag,
            updatedAt = updatedAt,
            createdAt = createdAt
          }

instance ToTType' BeamE.Estimate Estimate where
  toTType' Estimate {..} = do
    BeamE.EstimateT
      { BeamE.id = getId id,
        BeamE.requestId = getId requestId,
        BeamE.merchantId = getId <$> merchantId,
        BeamE.bppEstimateId = getId bppEstimateId,
        BeamE.itemId = itemId,
        BeamE.estimatedFare = realToFrac estimatedFare,
        BeamE.discount = realToFrac <$> discount,
        BeamE.estimatedTotalFare = realToFrac estimatedTotalFare,
        BeamE.minTotalFare = realToFrac totalFareRange.minFare,
        BeamE.maxTotalFare = realToFrac totalFareRange.maxFare,
        BeamE.estimatedDuration = estimatedDuration,
        BeamE.estimatedDistance = estimatedDistance,
        BeamE.device = device,
        BeamE.providerId = providerId,
        BeamE.providerUrl = showBaseUrl providerUrl,
        BeamE.providerName = providerName,
        BeamE.providerMobileNumber = providerMobileNumber,
        BeamE.providerCompletedRidesCount = providerCompletedRidesCount,
        BeamE.vehicleVariant = vehicleVariant,
        BeamE.driversLocation = driversLocation,
        BeamE.tripTermsId = getId <$> (tripTerms <&> (.id)),
        BeamE.nightShiftCharge = nightShiftInfo <&> (.nightShiftCharge),
        BeamE.oldNightShiftCharge = (.oldNightShiftCharge) =<< nightShiftInfo,
        BeamE.nightShiftStart = nightShiftInfo <&> (.nightShiftStart),
        BeamE.nightShiftEnd = nightShiftInfo <&> (.nightShiftEnd),
        BeamE.status = status,
        BeamE.waitingChargePerMin = waitingCharges.waitingChargePerMin,
        BeamE.specialLocationTag = specialLocationTag,
        BeamE.updatedAt = updatedAt,
        BeamE.createdAt = createdAt
      }
