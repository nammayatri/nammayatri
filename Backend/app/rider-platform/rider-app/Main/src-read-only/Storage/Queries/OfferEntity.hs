{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OfferEntity where

import qualified Domain.Types.OfferEntity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.OfferEntity as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.OfferEntity.OfferEntity -> m ())
create = createWithKV

findByEntityIdAndEntityType :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Domain.Types.OfferEntity.EntityType -> m (Maybe Domain.Types.OfferEntity.OfferEntity))
findByEntityIdAndEntityType entityId entityType = do findOneWithKV [Se.And [Se.Is Beam.entityId $ Se.Eq entityId, Se.Is Beam.entityType $ Se.Eq entityType]]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.OfferEntity.OfferEntity -> m (Maybe Domain.Types.OfferEntity.OfferEntity))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

instance FromTType' Beam.OfferEntity Domain.Types.OfferEntity.OfferEntity where
  fromTType' (Beam.OfferEntityT {..}) = do
    pure $
      Just
        Domain.Types.OfferEntity.OfferEntity
          { amountSaved = amountSaved,
            autoApply = (Kernel.Prelude.fromMaybe False autoApply),
            createdAt = createdAt,
            discountAmount = discountAmount,
            entityId = entityId,
            entityType = entityType,
            id = Kernel.Types.Id.Id id,
            isHidden = (Kernel.Prelude.fromMaybe True isHidden),
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            offerCode = offerCode,
            offerDescription = offerDescription,
            offerId = offerId,
            offerSponsoredBy = offerSponsoredBy,
            offerTitle = offerTitle,
            offerTnc = offerTnc,
            payoutAmount = payoutAmount,
            postOfferAmount = postOfferAmount,
            updatedAt = updatedAt
          }

instance ToTType' Beam.OfferEntity Domain.Types.OfferEntity.OfferEntity where
  toTType' (Domain.Types.OfferEntity.OfferEntity {..}) = do
    Beam.OfferEntityT
      { Beam.amountSaved = amountSaved,
        Beam.autoApply = (Kernel.Prelude.Just autoApply),
        Beam.createdAt = createdAt,
        Beam.discountAmount = discountAmount,
        Beam.entityId = entityId,
        Beam.entityType = entityType,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isHidden = (Kernel.Prelude.Just isHidden),
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.offerCode = offerCode,
        Beam.offerDescription = offerDescription,
        Beam.offerId = offerId,
        Beam.offerSponsoredBy = offerSponsoredBy,
        Beam.offerTitle = offerTitle,
        Beam.offerTnc = offerTnc,
        Beam.payoutAmount = payoutAmount,
        Beam.postOfferAmount = postOfferAmount,
        Beam.updatedAt = updatedAt
      }
