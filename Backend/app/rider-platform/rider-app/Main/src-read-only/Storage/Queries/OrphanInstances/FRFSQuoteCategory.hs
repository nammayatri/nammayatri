{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FRFSQuoteCategory where

import qualified Domain.Types.FRFSQuoteCategory
import qualified Domain.Types.FRFSQuoteCategoryType
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import qualified Kernel.Types.Price
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FRFSQuoteCategory as Beam
import qualified Storage.Queries.Transformers.FRFSQuoteCategory

instance FromTType' Beam.FRFSQuoteCategory Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory where
  fromTType' (Beam.FRFSQuoteCategoryT {..}) = do
    pure $
      Just
        Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory
          { bppItemId = bppItemId,
            category = Kernel.Prelude.fromMaybe Domain.Types.FRFSQuoteCategoryType.ADULT category,
            categoryMeta = Storage.Queries.Transformers.FRFSQuoteCategory.mkQuoteCategoryMetadataWithOrder code title description tnc categoryOrder,
            finalPrice = Kernel.Prelude.fmap (Kernel.Types.Common.mkPrice currency) finalPrice,
            holdId = holdId,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            offeredPrice = Kernel.Types.Common.mkPrice currency offeredPrice,
            price = Kernel.Types.Common.mkPrice currency price,
            quoteId = Kernel.Types.Id.Id quoteId,
            seatIds = fmap (fmap Kernel.Types.Id.Id) seatIds,
            seatLabels = seatLabels,
            selectedQuantity = Kernel.Prelude.fromMaybe 0 selectedQuantity,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSQuoteCategory Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory where
  toTType' (Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory {..}) = do
    Beam.FRFSQuoteCategoryT
      { Beam.bppItemId = bppItemId,
        Beam.category = Kernel.Prelude.Just category,
        Beam.categoryOrder = categoryMeta >>= Domain.Types.FRFSQuoteCategory.categoryOrder,
        Beam.code = categoryMeta <&> Domain.Types.FRFSQuoteCategory.code,
        Beam.description = categoryMeta <&> Domain.Types.FRFSQuoteCategory.description,
        Beam.title = categoryMeta <&> Domain.Types.FRFSQuoteCategory.title,
        Beam.tnc = categoryMeta <&> Domain.Types.FRFSQuoteCategory.tnc,
        Beam.finalPrice = Kernel.Prelude.fmap Kernel.Types.Price.getPriceAmount finalPrice,
        Beam.holdId = holdId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.offeredPrice = Kernel.Types.Price.getPriceAmount offeredPrice,
        Beam.currency = (Kernel.Prelude.Just . Kernel.Types.Price.getPriceCurrency) price,
        Beam.price = Kernel.Types.Price.getPriceAmount price,
        Beam.quoteId = Kernel.Types.Id.getId quoteId,
        Beam.seatIds = fmap Kernel.Types.Id.getId <$> seatIds,
        Beam.seatLabels = seatLabels,
        Beam.selectedQuantity = Kernel.Prelude.Just selectedQuantity,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
