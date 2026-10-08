{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSPassTicketStatistics where

import qualified Data.Time
import qualified Data.Time.Calendar
import qualified Domain.Types.FRFSPassTicketStatistics
import qualified Domain.Types.PurchasedPassPayment
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSPassTicketStatistics as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSPassTicketStatistics.FRFSPassTicketStatistics -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSPassTicketStatistics.FRFSPassTicketStatistics] -> m ())
createMany = traverse_ create

findAllByPurchasedPassPaymentId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.PurchasedPassPayment.PurchasedPassPayment -> m ([Domain.Types.FRFSPassTicketStatistics.FRFSPassTicketStatistics]))
findAllByPurchasedPassPaymentId purchasedPassPaymentId = do findAllWithKV [Se.Is Beam.purchasedPassPaymentId $ Se.Eq (Kernel.Types.Id.getId purchasedPassPaymentId)]

updateUsageByPurchasedPassPaymentIdAndDate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Domain.Types.PurchasedPassPayment.PurchasedPassPayment -> Data.Time.Calendar.Day -> m ())
updateUsageByPurchasedPassPaymentIdAndDate ticketCount fareAmount savedAmount purchasedPassPaymentId date = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.ticketCount ticketCount,
      Se.Set Beam.fareAmount fareAmount,
      Se.Set Beam.savedAmount savedAmount,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.purchasedPassPaymentId $ Se.Eq (Kernel.Types.Id.getId purchasedPassPaymentId), Se.Is Beam.date $ Se.Eq date]]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Data.Time.Calendar.Day -> Kernel.Types.Id.Id Domain.Types.PurchasedPassPayment.PurchasedPassPayment -> m (Maybe Domain.Types.FRFSPassTicketStatistics.FRFSPassTicketStatistics))
findByPrimaryKey date purchasedPassPaymentId = do findOneWithKV [Se.And [Se.Is Beam.date $ Se.Eq date, Se.Is Beam.purchasedPassPaymentId $ Se.Eq (Kernel.Types.Id.getId purchasedPassPaymentId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSPassTicketStatistics.FRFSPassTicketStatistics -> m ())
updateByPrimaryKey (Domain.Types.FRFSPassTicketStatistics.FRFSPassTicketStatistics {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.fareAmount fareAmount,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.personId (Kernel.Types.Id.getId personId),
      Se.Set Beam.savedAmount savedAmount,
      Se.Set Beam.ticketCount ticketCount,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.date $ Se.Eq date, Se.Is Beam.purchasedPassPaymentId $ Se.Eq (Kernel.Types.Id.getId purchasedPassPaymentId)]]

instance FromTType' Beam.FRFSPassTicketStatistics Domain.Types.FRFSPassTicketStatistics.FRFSPassTicketStatistics where
  fromTType' (Beam.FRFSPassTicketStatisticsT {..}) = do
    pure $
      Just
        Domain.Types.FRFSPassTicketStatistics.FRFSPassTicketStatistics
          { createdAt = createdAt,
            date = date,
            fareAmount = fareAmount,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            personId = Kernel.Types.Id.Id personId,
            purchasedPassPaymentId = Kernel.Types.Id.Id purchasedPassPaymentId,
            savedAmount = savedAmount,
            ticketCount = ticketCount,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSPassTicketStatistics Domain.Types.FRFSPassTicketStatistics.FRFSPassTicketStatistics where
  toTType' (Domain.Types.FRFSPassTicketStatistics.FRFSPassTicketStatistics {..}) = do
    Beam.FRFSPassTicketStatisticsT
      { Beam.createdAt = createdAt,
        Beam.date = date,
        Beam.fareAmount = fareAmount,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.purchasedPassPaymentId = Kernel.Types.Id.getId purchasedPassPaymentId,
        Beam.savedAmount = savedAmount,
        Beam.ticketCount = ticketCount,
        Beam.updatedAt = updatedAt
      }
