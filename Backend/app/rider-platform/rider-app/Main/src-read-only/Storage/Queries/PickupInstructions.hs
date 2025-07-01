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

findByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.PickupInstructions.PickupInstructions))
findByPersonId personId = do findOneWithKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

updatePickupInstructionsByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Kernel.Prelude.Text] -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updatePickupInstructionsByPersonId pickupInstructions personId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.pickupInstructions pickupInstructions, Se.Set Beam.updatedAt _now] [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

instance FromTType' Beam.PickupInstructions Domain.Types.PickupInstructions.PickupInstructions where
  fromTType' (Beam.PickupInstructionsT {..}) = do
    pure $
      Just
        Domain.Types.PickupInstructions.PickupInstructions
          { createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            personId = Kernel.Types.Id.Id personId,
            pickupInstructions = pickupInstructions,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PickupInstructions Domain.Types.PickupInstructions.PickupInstructions where
  toTType' (Domain.Types.PickupInstructions.PickupInstructions {..}) = do
    Beam.PickupInstructionsT
      { Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.pickupInstructions = pickupInstructions,
        Beam.updatedAt = updatedAt
      }
