{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CancellationCharges where

import qualified Domain.Types.CancellationCharges
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CancellationCharges as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CancellationCharges.CancellationCharges -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.CancellationCharges.CancellationCharges] -> m ())
createMany = traverse_ create

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.CancellationCharges.CancellationCharges -> m (Maybe Domain.Types.CancellationCharges.CancellationCharges))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CancellationCharges.CancellationCharges -> m ())
updateByPrimaryKey (Domain.Types.CancellationCharges.CancellationCharges {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.cancellationCharges cancellationCharges,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.rideId (Kernel.Types.Id.getId <$> rideId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.CancellationCharges Domain.Types.CancellationCharges.CancellationCharges where
  fromTType' (Beam.CancellationChargesT {..}) = do
    pure $
      Just
        Domain.Types.CancellationCharges.CancellationCharges
          { cancellationCharges = cancellationCharges,
            driverId = Kernel.Types.Id.Id driverId,
            id = Kernel.Types.Id.Id id,
            rideId = Kernel.Types.Id.Id <$> rideId
          }

instance ToTType' Beam.CancellationCharges Domain.Types.CancellationCharges.CancellationCharges where
  toTType' (Domain.Types.CancellationCharges.CancellationCharges {..}) = do
    Beam.CancellationChargesT
      { Beam.cancellationCharges = cancellationCharges,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.rideId = Kernel.Types.Id.getId <$> rideId
      }
