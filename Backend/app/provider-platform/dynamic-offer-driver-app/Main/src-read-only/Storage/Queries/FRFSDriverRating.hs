{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSDriverRating (module Storage.Queries.FRFSDriverRating, module ReExport) where

import qualified Data.Text
import qualified Domain.Types.FRFSDriverRating
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSDriverRating as Beam
import Storage.Queries.FRFSDriverRatingExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSDriverRating.FRFSDriverRating -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSDriverRating.FRFSDriverRating] -> m ())
createMany = traverse_ create

findAllByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.FRFSDriverRating.FRFSDriverRating]))
findAllByDriverId driverId = do findAllWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findByBookingId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Data.Text.Text -> m (Maybe Domain.Types.FRFSDriverRating.FRFSDriverRating))
findByBookingId bookingId = do findOneWithKV [Se.Is Beam.bookingId $ Se.Eq bookingId]

updateRating ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Types.Id.Id Domain.Types.FRFSDriverRating.FRFSDriverRating -> m ())
updateRating driverRatingValue fleetRatingValue feedbackDetails id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.driverRatingValue driverRatingValue,
      Se.Set Beam.fleetRatingValue fleetRatingValue,
      Se.Set Beam.feedbackDetails feedbackDetails,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSDriverRating.FRFSDriverRating -> m (Maybe Domain.Types.FRFSDriverRating.FRFSDriverRating))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSDriverRating.FRFSDriverRating -> m ())
updateByPrimaryKey (Domain.Types.FRFSDriverRating.FRFSDriverRating {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bookingId bookingId,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.driverRatingValue driverRatingValue,
      Se.Set Beam.feedbackDetails feedbackDetails,
      Se.Set Beam.fleetNumber fleetNumber,
      Se.Set Beam.fleetRatingValue fleetRatingValue,
      Se.Set Beam.gtfsId gtfsId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.operatorBadgeToken operatorBadgeToken,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
