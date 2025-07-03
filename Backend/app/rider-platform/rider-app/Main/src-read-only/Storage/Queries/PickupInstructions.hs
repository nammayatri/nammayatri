{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PickupInstructions where

import qualified Domain.Types.Person
import qualified Domain.Types.PickupInstructions
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PickupInstructions as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PickupInstructions.PickupInstructions -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PickupInstructions.PickupInstructions] -> m ())
createMany = traverse_ create

deleteById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.PickupInstructions.PickupInstructions -> m ())
deleteById id = do deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

deleteByPersonIdAndLocation :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Double -> Kernel.Prelude.Double -> m ())
deleteByPersonIdAndLocation personId lat lon = do deleteWithKV [Se.And [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId), Se.Is Beam.lat $ Se.Eq lat, Se.Is Beam.lon $ Se.Eq lon]]

findByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.PickupInstructions.PickupInstructions]))
findByPersonId personId = do findAllWithKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

findOldestByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.PickupInstructions.PickupInstructions]))
findOldestByPersonId personId = do findAllWithKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

updateByPersonIdAndLocation ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Double -> Kernel.Prelude.Double -> Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateByPersonIdAndLocation lat lon instruction personId = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.lat lat, Se.Set Beam.lon lon, Se.Set Beam.instruction instruction, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId),
          Se.Is Beam.lat $ Se.Eq lat,
          Se.Is Beam.lon $ Se.Eq lon
        ]
    ]

instance FromTType' Beam.PickupInstructions Domain.Types.PickupInstructions.PickupInstructions where
  fromTType' (Beam.PickupInstructionsT {..}) = do
    pure $
      Just
        Domain.Types.PickupInstructions.PickupInstructions
          { createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            instruction = instruction,
            lat = lat,
            lon = lon,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            personId = Kernel.Types.Id.Id personId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PickupInstructions Domain.Types.PickupInstructions.PickupInstructions where
  toTType' (Domain.Types.PickupInstructions.PickupInstructions {..}) = do
    Beam.PickupInstructionsT
      { Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.instruction = instruction,
        Beam.lat = lat,
        Beam.lon = lon,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.updatedAt = updatedAt
      }
