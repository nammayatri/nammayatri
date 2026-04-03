{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.FRFSQuoteCategory where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.FRFSQuoteCategory
import qualified Storage.Beam.FRFSQuoteCategory as Beam
import qualified Kernel.Prelude
import qualified Domain.Types.FRFSQuoteCategoryType
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Storage.Queries.Transformers.FRFSQuoteCategory



instance FromTType' Beam.FRFSQuoteCategory Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory
    where fromTType' (Beam.FRFSQuoteCategoryT {..}) = do pure $ Just Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory{bppItemId = bppItemId,
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
                                                                                                                      updatedAt = updatedAt}
instance ToTType' Beam.FRFSQuoteCategory Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory
    where toTType' (Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory {..}) = do Beam.FRFSQuoteCategoryT{Beam.bppItemId = bppItemId,
                                                                                                        Beam.category = Kernel.Prelude.Just category,
                                                                                                        Beam.categoryOrder = (categoryMeta >>= (.categoryOrder)),
                                                                                                        Beam.code = (categoryMeta <&> (.code)),
                                                                                                        Beam.description = (categoryMeta <&> (.description)),
                                                                                                        Beam.title = (categoryMeta <&> (.title)),
                                                                                                        Beam.tnc = (categoryMeta <&> (.tnc)),
                                                                                                        Beam.finalPrice = (Kernel.Prelude.fmap (.amount)) finalPrice,
                                                                                                        Beam.holdId = holdId,
                                                                                                        Beam.id = Kernel.Types.Id.getId id,
                                                                                                        Beam.merchantId = Kernel.Types.Id.getId merchantId,
                                                                                                        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
                                                                                                        Beam.offeredPrice = (.amount) offeredPrice,
                                                                                                        Beam.currency = ((Kernel.Prelude.Just . (.currency))) price,
                                                                                                        Beam.price = (.amount) price,
                                                                                                        Beam.quoteId = Kernel.Types.Id.getId quoteId,
                                                                                                        Beam.seatIds = (fmap Kernel.Types.Id.getId) <$> seatIds,
                                                                                                        Beam.seatLabels = seatLabels,
                                                                                                        Beam.selectedQuantity = Kernel.Prelude.Just selectedQuantity,
                                                                                                        Beam.createdAt = createdAt,
                                                                                                        Beam.updatedAt = updatedAt}



