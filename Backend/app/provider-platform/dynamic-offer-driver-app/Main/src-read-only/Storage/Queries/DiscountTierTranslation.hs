{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DiscountTierTranslation where

import qualified Domain.Types.DiscountTier
import qualified Domain.Types.DiscountTierTranslation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DiscountTierTranslation as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DiscountTierTranslation.DiscountTierTranslation -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DiscountTierTranslation.DiscountTierTranslation] -> m ())
createMany = traverse_ create

findAllByTierId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DiscountTier.DiscountTier -> m ([Domain.Types.DiscountTierTranslation.DiscountTierTranslation]))
findAllByTierId tierId = do findAllWithKV [Se.Is Beam.tierId $ Se.Eq (Kernel.Types.Id.getId tierId)]

findByTierIdAndLanguage ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.DiscountTier.DiscountTier -> Kernel.External.Types.Language -> m (Maybe Domain.Types.DiscountTierTranslation.DiscountTierTranslation))
findByTierIdAndLanguage tierId language = do findOneWithKV [Se.And [Se.Is Beam.tierId $ Se.Eq (Kernel.Types.Id.getId tierId), Se.Is Beam.language $ Se.Eq language]]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.External.Types.Language -> Kernel.Types.Id.Id Domain.Types.DiscountTier.DiscountTier -> m (Maybe Domain.Types.DiscountTierTranslation.DiscountTierTranslation))
findByPrimaryKey language tierId = do findOneWithKV [Se.And [Se.Is Beam.language $ Se.Eq language, Se.Is Beam.tierId $ Se.Eq (Kernel.Types.Id.getId tierId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DiscountTierTranslation.DiscountTierTranslation -> m ())
updateByPrimaryKey (Domain.Types.DiscountTierTranslation.DiscountTierTranslation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.description description, Se.Set Beam.name name, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.language $ Se.Eq language,
          Se.Is Beam.tierId $ Se.Eq (Kernel.Types.Id.getId tierId)
        ]
    ]

instance FromTType' Beam.DiscountTierTranslation Domain.Types.DiscountTierTranslation.DiscountTierTranslation where
  fromTType' (Beam.DiscountTierTranslationT {..}) = do
    pure $
      Just
        Domain.Types.DiscountTierTranslation.DiscountTierTranslation
          { description = description,
            language = language,
            name = name,
            tierId = Kernel.Types.Id.Id tierId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DiscountTierTranslation Domain.Types.DiscountTierTranslation.DiscountTierTranslation where
  toTType' (Domain.Types.DiscountTierTranslation.DiscountTierTranslation {..}) = do
    Beam.DiscountTierTranslationT
      { Beam.description = description,
        Beam.language = language,
        Beam.name = name,
        Beam.tierId = Kernel.Types.Id.getId tierId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
