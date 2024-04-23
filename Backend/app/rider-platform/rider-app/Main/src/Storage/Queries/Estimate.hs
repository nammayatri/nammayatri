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

import Data.Text (strip)
import Domain.Types.Estimate as DE
import Domain.Types.SearchRequest
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id (Id (Id, getId))
import Kernel.Utils.Common
import Kernel.Utils.Version
import qualified Sequelize as Se
import qualified Storage.Beam.Estimate as BeamE
import qualified Storage.Queries.EstimateBreakup as QEB
import qualified Storage.Queries.TripTerms as QTT

createEstimate :: (MonadFlow m, EsqDBFlow m r) => Estimate -> m ()
createEstimate = createWithKV

create :: (MonadFlow m, EsqDBFlow m r) => Estimate -> m ()
create estimate = do
  _ <- traverse_ QTT.createTripTerms estimate.tripTerms
  _ <- createEstimate estimate
  traverse_ QEB.create estimate.estimateBreakupList

createMany :: (MonadFlow m, EsqDBFlow m r) => [Estimate] -> m ()
createMany = traverse_ create

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Estimate -> m (Maybe Estimate)
findById (Id estimateId) = findOneWithKV [Se.Is BeamE.id $ Se.Eq estimateId]

findAllBySRId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id SearchRequest -> m [Estimate]
findAllBySRId (Id searchRequestId) = findAllWithKV [Se.Is BeamE.requestId $ Se.Eq searchRequestId]

findByBPPEstimateId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id BPPEstimate -> m (Maybe Estimate)
findByBPPEstimateId (Id bppEstimateId_) = findOneWithKV [Se.Is BeamE.bppEstimateId $ Se.Eq bppEstimateId_]

updateStatus :: (MonadFlow m, EsqDBFlow m r) => Id Estimate -> EstimateStatus -> m ()
updateStatus (Id estimateId) status_ = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamE.updatedAt now,
      Se.Set BeamE.status status_
    ]
    [Se.Is BeamE.id (Se.Eq estimateId)]

getStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Estimate -> m (Maybe EstimateStatus)
getStatus (Id estimateId) = findOneWithKV [Se.Is BeamE.id $ Se.Eq estimateId] <&> (DE.status <$>)

updateStatusByRequestId :: (MonadFlow m, EsqDBFlow m r) => Id SearchRequest -> EstimateStatus -> m ()
updateStatusByRequestId (Id searchId) status_ = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamE.updatedAt now,
      Se.Set BeamE.status status_
    ]
    [Se.Is BeamE.requestId (Se.Eq searchId)]

instance FromTType' BeamE.Estimate Estimate where
  fromTType' BeamE.EstimateT {..} = do
    clientBundleVersion' <- mapM readVersion (strip <$> clientBundleVersion)
    clientSdkVersion' <- mapM readVersion (strip <$> clientSdkVersion)
    clientConfigVersion' <- mapM readVersion (strip <$> clientConfigVersion)
    backendConfigVersion' <- mapM readVersion (strip <$> backendConfigVersion)

    etB <- QEB.findAllByEstimateIdT (Id id)
    trip <- if isJust tripTermsId then QTT.findById'' (Id (fromJust tripTermsId)) else pure Nothing
    pUrl <- parseBaseUrl providerUrl
    let totalFareRange =
          DE.FareRange
            { minFare = mkPrice currency minTotalFare,
              maxFare = mkPrice currency maxTotalFare
            }
    pure $
      Just $
        Estimate
          { id = Id id,
            requestId = Id requestId,
            merchantId = Id <$> merchantId,
            merchantOperatingCityId = Id <$> merchantOperatingCityId,
            bppEstimateId = Id bppEstimateId,
            itemId = itemId,
            discount = mkPrice currency <$> discount,
            estimatedFare = mkPrice currency estimatedFare,
            estimatedTotalFare = mkPrice currency estimatedTotalFare,
            totalFareRange = totalFareRange,
            estimatedDuration = estimatedDuration,
            estimatedDistance = estimatedDistance,
            device = device,
            providerId = providerId,
            providerUrl = pUrl,
            providerName = providerName,
            providerMobileNumber = providerMobileNumber,
            providerCompletedRidesCount = providerCompletedRidesCount,
            vehicleServiceTierType = vehicleVariant,
            serviceTierName = serviceTierName,
            serviceTierShortDesc = serviceTierShortDesc,
            tripTerms = trip,
            estimateBreakupList = etB,
            nightShiftInfo =
              ((,,) <$> nightShiftCharge <*> nightShiftStart <*> nightShiftEnd)
                <&> \(nightShiftCharge', nightShiftStart', nightShiftEnd') ->
                  DE.NightShiftInfo
                    { nightShiftCharge = mkPriceWithDefault nightShiftChargeAmount currency nightShiftCharge',
                      oldNightShiftCharge = oldNightShiftCharge,
                      nightShiftStart = nightShiftStart',
                      nightShiftEnd = nightShiftEnd'
                    },
            status = status,
            waitingCharges = DE.WaitingCharges $ mkPriceWithDefault waitingChargePerMinAmount currency <$> waitingChargePerMin,
            driversLocation = driversLocation,
            specialLocationTag = specialLocationTag,
            clientDevice = mkClientDevice clientOsType clientOsVersion,
            clientBundleVersion = clientBundleVersion',
            clientSdkVersion = clientSdkVersion',
            clientConfigVersion = clientConfigVersion',
            backendConfigVersion = backendConfigVersion',
            backendAppVersion = backendAppVersion,
            updatedAt = updatedAt,
            createdAt = createdAt,
            validTill = validTill
          }

instance ToTType' BeamE.Estimate Estimate where
  toTType' Estimate {..} = do
    BeamE.EstimateT
      { BeamE.id = getId id,
        BeamE.requestId = getId requestId,
        BeamE.merchantId = getId <$> merchantId,
        BeamE.merchantOperatingCityId = getId <$> merchantOperatingCityId,
        BeamE.bppEstimateId = getId bppEstimateId,
        BeamE.itemId = itemId,
        BeamE.estimatedFare = estimatedFare.amount,
        BeamE.discount = discount <&> (.amount),
        BeamE.estimatedTotalFare = estimatedTotalFare.amount,
        BeamE.currency = Just estimatedFare.currency,
        BeamE.minTotalFare = totalFareRange.minFare.amount,
        BeamE.maxTotalFare = totalFareRange.maxFare.amount,
        BeamE.estimatedDuration = estimatedDuration,
        BeamE.estimatedDistance = estimatedDistance,
        BeamE.device = device,
        BeamE.providerId = providerId,
        BeamE.providerUrl = showBaseUrl providerUrl,
        BeamE.providerName = providerName,
        BeamE.providerMobileNumber = providerMobileNumber,
        BeamE.providerCompletedRidesCount = providerCompletedRidesCount,
        BeamE.vehicleVariant = vehicleServiceTierType,
        BeamE.serviceTierName = serviceTierName,
        BeamE.serviceTierShortDesc = serviceTierShortDesc,
        BeamE.driversLocation = driversLocation,
        BeamE.tripTermsId = getId <$> (tripTerms <&> (.id)),
        BeamE.nightShiftCharge = nightShiftInfo <&> (.nightShiftCharge.amountInt),
        BeamE.nightShiftChargeAmount = nightShiftInfo <&> (.nightShiftCharge.amount),
        BeamE.oldNightShiftCharge = (.oldNightShiftCharge) =<< nightShiftInfo,
        BeamE.nightShiftStart = nightShiftInfo <&> (.nightShiftStart),
        BeamE.nightShiftEnd = nightShiftInfo <&> (.nightShiftEnd),
        BeamE.status = status,
        BeamE.waitingChargePerMin = waitingCharges.waitingChargePerMin <&> (.amountInt),
        BeamE.waitingChargePerMinAmount = waitingCharges.waitingChargePerMin <&> (.amount),
        BeamE.specialLocationTag = specialLocationTag,
        BeamE.clientOsType = clientDevice <&> (.deviceType),
        BeamE.clientOsVersion = clientDevice <&> (.deviceVersion),
        BeamE.clientBundleVersion = versionToText <$> clientBundleVersion,
        BeamE.clientSdkVersion = versionToText <$> clientSdkVersion,
        BeamE.clientConfigVersion = versionToText <$> clientConfigVersion,
        BeamE.backendConfigVersion = versionToText <$> backendConfigVersion,
        BeamE.backendAppVersion = backendAppVersion,
        BeamE.updatedAt = updatedAt,
        BeamE.createdAt = createdAt,
        BeamE.validTill = validTill
      }
