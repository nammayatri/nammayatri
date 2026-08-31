{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverFyEarnings where

import qualified Domain.Types.DriverFyEarnings
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverFyEarnings as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverFyEarnings.DriverFyEarnings -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DriverFyEarnings.DriverFyEarnings] -> m ())
createMany = traverse_ create

findAllByPersonIdAndFinancialYear ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Int -> m ([Domain.Types.DriverFyEarnings.DriverFyEarnings]))
findAllByPersonIdAndFinancialYear personId financialYear = do findAllWithKV [Se.And [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId), Se.Is Beam.financialYear $ Se.Eq financialYear]]

findByPersonIdAndFinancialYearAndQuarter ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> m (Maybe Domain.Types.DriverFyEarnings.DriverFyEarnings))
findByPersonIdAndFinancialYearAndQuarter personId financialYear quarter = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId),
          Se.Is Beam.financialYear $ Se.Eq financialYear,
          Se.Is Beam.quarter $ Se.Eq quarter
        ]
    ]

updateEarningsTotals ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> m ())
updateEarningsTotals netEarningsTotal tdsAmountTotal personId financialYear quarter = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.netEarningsTotal netEarningsTotal,
      Se.Set Beam.tdsAmountTotal tdsAmountTotal,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId), Se.Is Beam.financialYear $ Se.Eq financialYear, Se.Is Beam.quarter $ Se.Eq quarter]]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DriverFyEarnings.DriverFyEarnings -> m (Maybe Domain.Types.DriverFyEarnings.DriverFyEarnings))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverFyEarnings.DriverFyEarnings -> m ())
updateByPrimaryKey (Domain.Types.DriverFyEarnings.DriverFyEarnings {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.financialYear financialYear,
      Se.Set Beam.netEarningsTotal netEarningsTotal,
      Se.Set Beam.personId (Kernel.Types.Id.getId personId),
      Se.Set Beam.quarter quarter,
      Se.Set Beam.tdsAmountTotal tdsAmountTotal,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.DriverFyEarnings Domain.Types.DriverFyEarnings.DriverFyEarnings where
  fromTType' (Beam.DriverFyEarningsT {..}) = do
    pure $
      Just
        Domain.Types.DriverFyEarnings.DriverFyEarnings
          { financialYear = financialYear,
            id = Kernel.Types.Id.Id id,
            netEarningsTotal = netEarningsTotal,
            personId = Kernel.Types.Id.Id personId,
            quarter = quarter,
            tdsAmountTotal = tdsAmountTotal,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DriverFyEarnings Domain.Types.DriverFyEarnings.DriverFyEarnings where
  toTType' (Domain.Types.DriverFyEarnings.DriverFyEarnings {..}) = do
    Beam.DriverFyEarningsT
      { Beam.financialYear = financialYear,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.netEarningsTotal = netEarningsTotal,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.quarter = quarter,
        Beam.tdsAmountTotal = tdsAmountTotal,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
