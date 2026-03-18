module Storage.Queries.DriverFinancialStatementExtra where

import qualified Domain.Types.DriverFinancialStatement as Domain
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person (Person)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.DriverFinancialStatement as Beam
import Storage.Queries.OrphanInstances.DriverFinancialStatement ()

-- Extra code goes here --

findAllByDriverIdWithLimitAndOffset ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Person ->
  Int ->
  Int ->
  m [Domain.DriverFinancialStatement]
findAllByDriverIdWithLimitAndOffset driverId limit offset =
  findAllWithOptionsKV
    [Se.Is Beam.driverId $ Se.Eq (getId driverId)]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)

findAllByDriverIdAndStatus ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Person ->
  Domain.StatementStatus ->
  m [Domain.DriverFinancialStatement]
findAllByDriverIdAndStatus driverId status =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (getId driverId),
          Se.Is Beam.status $ Se.Eq status
        ]
    ]

findAllByMerchantIdAndPeriod ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DM.Merchant ->
  UTCTime ->
  UTCTime ->
  m [Domain.DriverFinancialStatement]
findAllByMerchantIdAndPeriod merchantId periodStart periodEnd =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (getId merchantId),
          Se.Is Beam.periodStart $ Se.Eq periodStart,
          Se.Is Beam.periodEnd $ Se.Eq periodEnd
        ]
    ]
