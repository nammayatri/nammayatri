{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BookingCancellationReasonExtra where

import qualified Data.List
import qualified Database.Beam as B
import Domain.Types.Booking
import Domain.Types.BookingCancellationReason as DBCR
import Domain.Types.CancellationReason (CancellationReasonCode (..))
import Domain.Types.Person
import Domain.Types.Ride
import qualified EulerHS.Language as L
import EulerHS.Prelude as P hiding (null, (^.))
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.External.Maps.Types (LatLong (..), lat, lon)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.BookingCancellationReason as BeamBCR
import qualified Storage.Beam.Common as BeamCommon
import Storage.Queries.OrphanInstances.BookingCancellationReason

-- Extra code goes here --

findAllCancelledByDriverId :: KvDbFlow m r => Id Person -> m Int
findAllCancelledByDriverId driverId = do
  dbConf <- getMasterBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.aggregate_ (\_ -> B.as_ @Int B.countAll_) $
          B.filter_'
            ( \bcr ->
                B.sqlBool_ (bcr.source B.==. B.val_ ByDriver)
                  B.&&?. (bcr.driverId B.==?. B.val_ (Just $ getId driverId))
            )
            do
              B.all_ (BeamCommon.bookingCancellationReason BeamCommon.atlasDB)
  pure $ either (const 0) (\r -> if Data.List.null r then 0 else Data.List.head r) res

upsert :: KvDbFlow m r => BookingCancellationReason -> m ()
upsert cancellationReason = do
  res <- findOneWithKV [Se.Is BeamBCR.bookingId $ Se.Eq (getId cancellationReason.bookingId)]
  if isJust res
    then
      updateOneWithKV
        [ Se.Set BeamBCR.bookingId (getId cancellationReason.bookingId),
          Se.Set BeamBCR.rideId (getId <$> cancellationReason.rideId),
          Se.Set BeamBCR.reasonCode ((\(CancellationReasonCode x) -> x) <$> cancellationReason.reasonCode),
          Se.Set BeamBCR.additionalInfo cancellationReason.additionalInfo
        ]
        [Se.Is BeamBCR.bookingId (Se.Eq $ getId cancellationReason.bookingId)]
    else createWithKV cancellationReason

findAllBookingIdsCancelledByDriverId :: KvDbFlow m r => Id Person -> m [Id Booking]
findAllBookingIdsCancelledByDriverId driverId = findAllWithDb [Se.And [Se.Is BeamBCR.driverId $ Se.Eq (Just $ getId driverId), Se.Is BeamBCR.source $ Se.Eq ByDriver]] <&> (DBCR.bookingId <$>)
