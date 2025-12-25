{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.HotSpotConfig where

import qualified Domain.Types.HotSpotConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.HotSpotConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.HotSpotConfig.HotSpotConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.HotSpotConfig.HotSpotConfig] -> m ())
createMany = traverse_ create

findConfigByMerchantId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.HotSpotConfig.HotSpotConfig -> m (Maybe Domain.Types.HotSpotConfig.HotSpotConfig))
findConfigByMerchantId id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.HotSpotConfig.HotSpotConfig -> m (Maybe Domain.Types.HotSpotConfig.HotSpotConfig))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.HotSpotConfig.HotSpotConfig -> m ())
updateByPrimaryKey (Domain.Types.HotSpotConfig.HotSpotConfig {..}) = do
  updateWithKV
    [ Se.Set Beam.blockRadius blockRadius,
      Se.Set Beam.hotSpotExpiry hotSpotExpiry,
      Se.Set Beam.hotSpotRadius hotSpotRadius,
      Se.Set Beam.maxGeoHashToFilter maxGeoHashToFilter,
      Se.Set Beam.maxNumHotSpotsToShow maxNumHotSpotsToShow,
      Se.Set Beam.minFrequencyOfHotSpot minFrequencyOfHotSpot,
      Se.Set Beam.precisionToFilterGeohash precisionToFilterGeohash,
      Se.Set Beam.precisionToGetGeohash precisionToGetGeohash,
      Se.Set Beam.precisionToSetGeohash precisionToSetGeohash,
      Se.Set Beam.shouldSaveSearchHotSpot shouldSaveSearchHotSpot,
      Se.Set Beam.shouldTakeHotSpot shouldTakeHotSpot,
      Se.Set Beam.weightOfAutoPickup weightOfAutoPickup,
      Se.Set Beam.weightOfAutoSaved weightOfAutoSaved,
      Se.Set Beam.weightOfManualPickup weightOfManualPickup,
      Se.Set Beam.weightOfManualSaved weightOfManualSaved,
      Se.Set Beam.weightOfSpecialLocation weightOfSpecialLocation,
      Se.Set Beam.weightOfTripEnd weightOfTripEnd,
      Se.Set Beam.weightOfTripStart weightOfTripStart
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.HotSpotConfig Domain.Types.HotSpotConfig.HotSpotConfig where
  fromTType' (Beam.HotSpotConfigT {..}) = do
    pure $
      Just
        Domain.Types.HotSpotConfig.HotSpotConfig
          { blockRadius = blockRadius,
            hotSpotExpiry = hotSpotExpiry,
            hotSpotRadius = hotSpotRadius,
            id = Kernel.Types.Id.Id id,
            maxGeoHashToFilter = maxGeoHashToFilter,
            maxNumHotSpotsToShow = maxNumHotSpotsToShow,
            minFrequencyOfHotSpot = minFrequencyOfHotSpot,
            precisionToFilterGeohash = precisionToFilterGeohash,
            precisionToGetGeohash = precisionToGetGeohash,
            precisionToSetGeohash = precisionToSetGeohash,
            shouldSaveSearchHotSpot = shouldSaveSearchHotSpot,
            shouldTakeHotSpot = shouldTakeHotSpot,
            weightOfAutoPickup = weightOfAutoPickup,
            weightOfAutoSaved = weightOfAutoSaved,
            weightOfManualPickup = weightOfManualPickup,
            weightOfManualSaved = weightOfManualSaved,
            weightOfSpecialLocation = weightOfSpecialLocation,
            weightOfTripEnd = weightOfTripEnd,
            weightOfTripStart = weightOfTripStart
          }

instance ToTType' Beam.HotSpotConfig Domain.Types.HotSpotConfig.HotSpotConfig where
  toTType' (Domain.Types.HotSpotConfig.HotSpotConfig {..}) = do
    Beam.HotSpotConfigT
      { Beam.blockRadius = blockRadius,
        Beam.hotSpotExpiry = hotSpotExpiry,
        Beam.hotSpotRadius = hotSpotRadius,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.maxGeoHashToFilter = maxGeoHashToFilter,
        Beam.maxNumHotSpotsToShow = maxNumHotSpotsToShow,
        Beam.minFrequencyOfHotSpot = minFrequencyOfHotSpot,
        Beam.precisionToFilterGeohash = precisionToFilterGeohash,
        Beam.precisionToGetGeohash = precisionToGetGeohash,
        Beam.precisionToSetGeohash = precisionToSetGeohash,
        Beam.shouldSaveSearchHotSpot = shouldSaveSearchHotSpot,
        Beam.shouldTakeHotSpot = shouldTakeHotSpot,
        Beam.weightOfAutoPickup = weightOfAutoPickup,
        Beam.weightOfAutoSaved = weightOfAutoSaved,
        Beam.weightOfManualPickup = weightOfManualPickup,
        Beam.weightOfManualSaved = weightOfManualSaved,
        Beam.weightOfSpecialLocation = weightOfSpecialLocation,
        Beam.weightOfTripEnd = weightOfTripEnd,
        Beam.weightOfTripStart = weightOfTripStart
      }
