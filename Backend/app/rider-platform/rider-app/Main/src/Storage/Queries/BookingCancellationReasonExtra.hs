module Storage.Queries.BookingCancellationReasonExtra where

import qualified Database.Beam as B
import Domain.Types.Booking
import Domain.Types.BookingCancellationReason
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.BookingCancellationReason as BeamBCR
import qualified Storage.Beam.Common as BeamCommon
import Storage.Queries.OrphanInstances.BookingCancellationReason ()

-- Extra code goes here --

upsert :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => BookingCancellationReason -> m ()
upsert cancellationReason = do
  res <- findOneWithKV [Se.Is BeamBCR.bookingId $ Se.Eq (getId cancellationReason.bookingId)]
  if isJust res
    then
      updateOneWithKV
        [ Se.Set BeamBCR.bookingId (getId cancellationReason.bookingId),
          Se.Set BeamBCR.rideId (getId <$> cancellationReason.rideId),
          Se.Set BeamBCR.source cancellationReason.source,
          Se.Set BeamBCR.reasonCode cancellationReason.reasonCode,
          Se.Set BeamBCR.reasonStage cancellationReason.reasonStage,
          Se.Set BeamBCR.additionalInfo cancellationReason.additionalInfo
        ]
        [Se.Is BeamBCR.bookingId (Se.Eq $ getId cancellationReason.bookingId)]
    else createWithKV cancellationReason

countCancelledBookingsByBookingIds :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => [Id Booking] -> CancellationSource -> m Int
countCancelledBookingsByBookingIds bookingIds cancellationSource = do
  dbConf <- getReplicaBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.aggregate_ (\_ -> B.as_ @Int B.countAll_) $
          B.filter_
            (\BeamBCR.BookingCancellationReasonT {..} -> bookingId `B.in_` (B.val_ . getId <$> bookingIds) B.&&. (source B.==. B.val_ cancellationSource))
            do
              B.all_ (BeamCommon.bookingCancellationReason BeamCommon.atlasDB)
  pure $ either (const 0) (\r -> if null r then 0 else head r) res
