{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DiscountTranslation where

import qualified Domain.Types.Discount
import qualified Domain.Types.DiscountTranslation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DiscountTranslation as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DiscountTranslation.DiscountTranslation -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DiscountTranslation.DiscountTranslation] -> m ())
createMany = traverse_ create

findAllByDiscountId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Discount.Discount -> m ([Domain.Types.DiscountTranslation.DiscountTranslation]))
findAllByDiscountId discountId = do findAllWithKV [Se.Is Beam.discountId $ Se.Eq (Kernel.Types.Id.getId discountId)]

findByDiscountIdAndLanguage ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Discount.Discount -> Kernel.External.Types.Language -> m (Maybe Domain.Types.DiscountTranslation.DiscountTranslation))
findByDiscountIdAndLanguage discountId language = do findOneWithKV [Se.And [Se.Is Beam.discountId $ Se.Eq (Kernel.Types.Id.getId discountId), Se.Is Beam.language $ Se.Eq language]]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Discount.Discount -> Kernel.External.Types.Language -> m (Maybe Domain.Types.DiscountTranslation.DiscountTranslation))
findByPrimaryKey discountId language = do findOneWithKV [Se.And [Se.Is Beam.discountId $ Se.Eq (Kernel.Types.Id.getId discountId), Se.Is Beam.language $ Se.Eq language]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DiscountTranslation.DiscountTranslation -> m ())
updateByPrimaryKey (Domain.Types.DiscountTranslation.DiscountTranslation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.description description, Se.Set Beam.name name, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.discountId $ Se.Eq (Kernel.Types.Id.getId discountId),
          Se.Is Beam.language $ Se.Eq language
        ]
    ]

instance FromTType' Beam.DiscountTranslation Domain.Types.DiscountTranslation.DiscountTranslation where
  fromTType' (Beam.DiscountTranslationT {..}) = do
    pure $
      Just
        Domain.Types.DiscountTranslation.DiscountTranslation
          { description = description,
            discountId = Kernel.Types.Id.Id discountId,
            language = language,
            name = name,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DiscountTranslation Domain.Types.DiscountTranslation.DiscountTranslation where
  toTType' (Domain.Types.DiscountTranslation.DiscountTranslation {..}) = do
    Beam.DiscountTranslationT
      { Beam.description = description,
        Beam.discountId = Kernel.Types.Id.getId discountId,
        Beam.language = language,
        Beam.name = name,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
