{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Seat where

import qualified Domain.Types.Seat
import qualified Domain.Types.SeatLayout
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Seat as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Seat.Seat -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Seat.Seat] -> m ())
createMany = traverse_ create

findAllByLayoutId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SeatLayout.SeatLayout -> m ([Domain.Types.Seat.Seat]))
findAllByLayoutId seatLayoutId = do findAllWithKV [Se.Is Beam.seatLayoutId $ Se.Eq (Kernel.Types.Id.getId seatLayoutId)]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Seat.Seat -> m (Maybe Domain.Types.Seat.Seat))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Seat.Seat -> m (Maybe Domain.Types.Seat.Seat))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Seat.Seat -> m ())
updateByPrimaryKey (Domain.Types.Seat.Seat {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.colNo colNo,
      Se.Set Beam.isBookable isBookable,
      Se.Set Beam.isLadiesOnly isLadiesOnly,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.rowNo rowNo,
      Se.Set Beam.seatLabel seatLabel,
      Se.Set Beam.seatLayoutId (Kernel.Types.Id.getId seatLayoutId),
      Se.Set Beam.seatType seatType,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.Seat Domain.Types.Seat.Seat where
  fromTType' (Beam.SeatT {..}) = do
    pure $
      Just
        Domain.Types.Seat.Seat
          { colNo = colNo,
            id = Kernel.Types.Id.Id id,
            isBookable = isBookable,
            isLadiesOnly = isLadiesOnly,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            rowNo = rowNo,
            seatLabel = seatLabel,
            seatLayoutId = Kernel.Types.Id.Id seatLayoutId,
            seatType = seatType,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Seat Domain.Types.Seat.Seat where
  toTType' (Domain.Types.Seat.Seat {..}) = do
    Beam.SeatT
      { Beam.colNo = colNo,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isBookable = isBookable,
        Beam.isLadiesOnly = isLadiesOnly,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.rowNo = rowNo,
        Beam.seatLabel = seatLabel,
        Beam.seatLayoutId = Kernel.Types.Id.getId seatLayoutId,
        Beam.seatType = seatType,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
