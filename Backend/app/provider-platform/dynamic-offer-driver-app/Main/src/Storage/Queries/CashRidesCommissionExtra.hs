module Storage.Queries.CashRidesCommissionExtra where

import qualified Domain.Types.CashRidesCommission as DCashRidesCommission
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person as DP
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.CashRidesCommission as BeamCRC
import Storage.Queries.OrphanInstances.CashRidesCommission ()

-- Extra code goes here --

-- overlapping intervals: minRange <= tripEndTime < maxRange and lastSettlementTime <= tripEndTime < nextSettlementTime
findAllByPersonModeAndOverlappingTimeRange ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  DMPM.PaymentMode ->
  UTCTime ->
  UTCTime ->
  m [DCashRidesCommission.CashRidesCommission]
findAllByPersonModeAndOverlappingTimeRange personId paymentMode minRange maxRange = do
  findAllWithKV
    [ Se.And
        [ Se.Is BeamCRC.personId $ Se.Eq personId.getId,
          Se.Is BeamCRC.paymentMode $ Se.Eq paymentMode,
          Se.Is BeamCRC.lastSettlementTime $ Se.LessThan maxRange,
          Se.Is BeamCRC.nextSettlementTime $ Se.GreaterThan minRange
        ]
    ]
