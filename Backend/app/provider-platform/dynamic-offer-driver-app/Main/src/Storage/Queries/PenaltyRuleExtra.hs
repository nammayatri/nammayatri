module Storage.Queries.PenaltyRuleExtra where

import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PenaltyRule
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.PenaltyRule as Beam

-- Extra code goes here --

findAllActiveByMerchantIdAndTriggerEventSorted ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Merchant.Merchant ->
  Domain.Types.PenaltyRule.PenaltyTriggerEvent ->
  UTCTime ->
  m [Domain.Types.PenaltyRule.PenaltyRule]
findAllActiveByMerchantIdAndTriggerEventSorted merchantId triggerEvent now = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId),
          Se.Is Beam.triggerEvent $ Se.Eq triggerEvent,
          Se.Is Beam.isActive $ Se.Eq True,
          Se.Or
            [ Se.Is Beam.startDate $ Se.Eq Nothing,
              Se.Is Beam.startDate $ Se.LessThanOrEq (Just now)
            ],
          Se.Or
            [ Se.Is Beam.endDate $ Se.Eq Nothing,
              Se.Is Beam.endDate $ Se.GreaterThanOrEq (Just now)
            ]
        ]
    ]
    (Se.Asc Beam.priority)
    Nothing
    Nothing

findAllByMerchantIdWithLimitOffset ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Merchant.Merchant ->
  Maybe Int ->
  Maybe Int ->
  m [Domain.Types.PenaltyRule.PenaltyRule]
findAllByMerchantIdWithLimitOffset merchantId mbLimit mbOffset = do
  let limitVal = min 50 $ fromMaybe 10 mbLimit
      offsetVal = fromMaybe 0 mbOffset
  findAllWithOptionsKV
    [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)]
    (Se.Desc Beam.createdAt)
    (Just limitVal)
    (Just offsetVal)
