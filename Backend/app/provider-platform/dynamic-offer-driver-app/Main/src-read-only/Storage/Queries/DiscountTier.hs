{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DiscountTier where

import qualified Domain.Types.Discount
import qualified Domain.Types.DiscountTier
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DiscountTier as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DiscountTier.DiscountTier -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DiscountTier.DiscountTier] -> m ())
createMany = traverse_ create

findAllByDiscountId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Discount.Discount -> m [Domain.Types.DiscountTier.DiscountTier])
findAllByDiscountId discountId = do findAllWithKV [Se.Is Beam.discountId $ Se.Eq (Kernel.Types.Id.getId discountId)]

findAllByDiscountIdOrderByTier ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Discount.Discount -> m [Domain.Types.DiscountTier.DiscountTier])
findAllByDiscountIdOrderByTier limit offset discountId = do findAllWithOptionsKV [Se.Is Beam.discountId $ Se.Eq (Kernel.Types.Id.getId discountId)] (Se.Desc Beam.tierOrder) limit offset

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DiscountTier.DiscountTier -> m (Maybe Domain.Types.DiscountTier.DiscountTier))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DiscountTier.DiscountTier -> m ())
updateByPrimaryKey (Domain.Types.DiscountTier.DiscountTier {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.discountId (Kernel.Types.Id.getId discountId),
      Se.Set Beam.discountValue discountValue,
      Se.Set Beam.discountValueType discountValueType,
      Se.Set Beam.thresholdValue thresholdValue,
      Se.Set Beam.tierOrder tierOrder,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.DiscountTier Domain.Types.DiscountTier.DiscountTier where
  fromTType' (Beam.DiscountTierT {..}) = do
    pure $
      Just
        Domain.Types.DiscountTier.DiscountTier
          { discountId = Kernel.Types.Id.Id discountId,
            discountValue = discountValue,
            discountValueType = discountValueType,
            id = Kernel.Types.Id.Id id,
            thresholdValue = thresholdValue,
            tierOrder = tierOrder,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DiscountTier Domain.Types.DiscountTier.DiscountTier where
  toTType' (Domain.Types.DiscountTier.DiscountTier {..}) = do
    Beam.DiscountTierT
      { Beam.discountId = Kernel.Types.Id.getId discountId,
        Beam.discountValue = discountValue,
        Beam.discountValueType = discountValueType,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.thresholdValue = thresholdValue,
        Beam.tierOrder = tierOrder,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
