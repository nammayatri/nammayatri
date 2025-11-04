{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSQuoteCategory where

import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSQuoteCategory
import qualified Domain.Types.FRFSQuoteCategoryType
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSQuoteCategory as Beam
import qualified Storage.Queries.Transformers.FRFSQuoteCategory

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory] -> m ())
createMany = traverse_ create

findAllByQuoteId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> m [Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory])
findAllByQuoteId quoteId = do findAllWithKV [Se.Is Beam.quoteId $ Se.Eq (Kernel.Types.Id.getId quoteId)]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory -> m (Maybe Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateFinalPriceByQuoteCategoryId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Types.Common.Price -> Kernel.Types.Id.Id Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory -> m ())
updateFinalPriceByQuoteCategoryId finalPrice id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.finalPrice (Kernel.Prelude.fmap (.amount) finalPrice), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateQuantityByQuoteCategoryId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory -> m ())
updateQuantityByQuoteCategoryId selectedQuantity id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.selectedQuantity selectedQuantity, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory -> m (Maybe Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory -> m ())
updateByPrimaryKey (Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bppItemId bppItemId,
      Se.Set Beam.category (Kernel.Prelude.Just category),
      Se.Set Beam.code (categoryMeta <&> (.code)),
      Se.Set Beam.description (categoryMeta <&> (.description)),
      Se.Set Beam.title (categoryMeta <&> (.title)),
      Se.Set Beam.tnc (categoryMeta <&> (.tnc)),
      Se.Set Beam.finalPrice (Kernel.Prelude.fmap (.amount) finalPrice),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.offeredPrice ((.amount) offeredPrice),
      Se.Set Beam.currency ((Kernel.Prelude.Just . (.currency)) price),
      Se.Set Beam.price ((.amount) price),
      Se.Set Beam.quoteId (Kernel.Types.Id.getId quoteId),
      Se.Set Beam.selectedQuantity selectedQuantity,
      Se.Set Beam.ticketCategoryMetadataConfigId ticketCategoryMetadataConfigId,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FRFSQuoteCategory Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory where
  fromTType' (Beam.FRFSQuoteCategoryT {..}) = do
    pure $
      Just
        Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory
          { bppItemId = bppItemId,
            category = Kernel.Prelude.fromMaybe Domain.Types.FRFSQuoteCategoryType.ADULT category,
            categoryMeta = Storage.Queries.Transformers.FRFSQuoteCategory.mkQuoteCategoryMetadata code title description tnc,
            finalPrice = Kernel.Prelude.fmap (Kernel.Types.Common.mkPrice currency) finalPrice,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            offeredPrice = Kernel.Types.Common.mkPrice currency offeredPrice,
            price = Kernel.Types.Common.mkPrice currency price,
            quoteId = Kernel.Types.Id.Id quoteId,
            selectedQuantity = selectedQuantity,
            ticketCategoryMetadataConfigId = ticketCategoryMetadataConfigId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSQuoteCategory Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory where
  toTType' (Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory {..}) = do
    Beam.FRFSQuoteCategoryT
      { Beam.bppItemId = bppItemId,
        Beam.category = Kernel.Prelude.Just category,
        Beam.code = categoryMeta <&> (.code),
        Beam.description = categoryMeta <&> (.description),
        Beam.title = categoryMeta <&> (.title),
        Beam.tnc = categoryMeta <&> (.tnc),
        Beam.finalPrice = Kernel.Prelude.fmap (.amount) finalPrice,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.offeredPrice = (.amount) offeredPrice,
        Beam.currency = (Kernel.Prelude.Just . (.currency)) price,
        Beam.price = (.amount) price,
        Beam.quoteId = Kernel.Types.Id.getId quoteId,
        Beam.selectedQuantity = selectedQuantity,
        Beam.ticketCategoryMetadataConfigId = ticketCategoryMetadataConfigId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
