{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MultimodalPreferences where

import qualified BecknV2.FRFS.Enums
import qualified Domain.Types.Common
import qualified Domain.Types.MultimodalPreferences
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MultimodalPreferences as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MultimodalPreferences.MultimodalPreferences -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MultimodalPreferences.MultimodalPreferences] -> m ())
createMany = traverse_ create

findByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.MultimodalPreferences.MultimodalPreferences))
findByPersonId personId = do findOneWithKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

updateUserPreferences ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  ([Domain.Types.Common.MultimodalTravelMode] -> Domain.Types.MultimodalPreferences.JourneyOptionsSortingType -> Kernel.Prelude.Maybe [BecknV2.FRFS.Enums.ServiceTierType] -> Kernel.Prelude.Maybe [BecknV2.FRFS.Enums.ServiceTierType] -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateUserPreferences allowedTransitModes journeyOptionsSortingType busTransitTypes subwayTransitTypes personId = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.allowedTransitModes allowedTransitModes,
      Se.Set Beam.journeyOptionsSortingType journeyOptionsSortingType,
      Se.Set Beam.busTransitTypes busTransitTypes,
      Se.Set Beam.subwayTransitTypes subwayTransitTypes,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.MultimodalPreferences.MultimodalPreferences))
findByPrimaryKey personId = do findOneWithKV [Se.And [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MultimodalPreferences.MultimodalPreferences -> m ())
updateByPrimaryKey (Domain.Types.MultimodalPreferences.MultimodalPreferences {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.allowedTransitModes allowedTransitModes,
      Se.Set Beam.busTransitTypes busTransitTypes,
      Se.Set Beam.journeyOptionsSortingType journeyOptionsSortingType,
      Se.Set Beam.subwayTransitTypes subwayTransitTypes,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]]

instance FromTType' Beam.MultimodalPreferences Domain.Types.MultimodalPreferences.MultimodalPreferences where
  fromTType' (Beam.MultimodalPreferencesT {..}) = do
    pure $
      Just
        Domain.Types.MultimodalPreferences.MultimodalPreferences
          { allowedTransitModes = allowedTransitModes,
            busTransitTypes = busTransitTypes,
            journeyOptionsSortingType = journeyOptionsSortingType,
            personId = Kernel.Types.Id.Id personId,
            subwayTransitTypes = subwayTransitTypes,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.MultimodalPreferences Domain.Types.MultimodalPreferences.MultimodalPreferences where
  toTType' (Domain.Types.MultimodalPreferences.MultimodalPreferences {..}) = do
    Beam.MultimodalPreferencesT
      { Beam.allowedTransitModes = allowedTransitModes,
        Beam.busTransitTypes = busTransitTypes,
        Beam.journeyOptionsSortingType = journeyOptionsSortingType,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.subwayTransitTypes = subwayTransitTypes,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
