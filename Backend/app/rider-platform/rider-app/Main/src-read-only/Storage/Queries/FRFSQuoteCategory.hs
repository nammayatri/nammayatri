{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.FRFSQuoteCategory (module Storage.Queries.FRFSQuoteCategory, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.FRFSQuoteCategoryExtra as ReExport
import qualified Domain.Types.FRFSQuoteCategory
import qualified Storage.Beam.FRFSQuoteCategory as Beam
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Domain.Types.FRFSQuote
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory] -> m ())
createMany = traverse_ create
findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory -> m (Maybe Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
updateFinalPriceByQuoteCategoryId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                                     (Kernel.Prelude.Maybe Kernel.Types.Common.Price -> Kernel.Types.Id.Id Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory -> m ())
updateFinalPriceByQuoteCategoryId finalPrice id = do {_now <- getCurrentTime;
                                                      updateWithKV [Se.Set Beam.finalPrice ((Kernel.Prelude.fmap (.amount)) finalPrice), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]}
updateOfferedPriceByQuoteId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Common.Price -> Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> m ())
updateOfferedPriceByQuoteId offeredPrice quoteId = do {_now <- getCurrentTime;
                                                       updateWithKV [Se.Set Beam.offeredPrice ((.amount) offeredPrice), Se.Set Beam.updatedAt _now] [Se.Is Beam.quoteId $ Se.Eq (Kernel.Types.Id.getId quoteId)]}
updateQuantityByQuoteCategoryId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory -> m ())
updateQuantityByQuoteCategoryId selectedQuantity id = do {_now <- getCurrentTime;
                                                          updateWithKV [Se.Set Beam.selectedQuantity (Kernel.Prelude.Just selectedQuantity), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]}
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory -> m (Maybe Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory -> m ())
updateByPrimaryKey (Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory {..}) = do {_now <- getCurrentTime;
                                                                                 updateWithKV [Se.Set Beam.bppItemId bppItemId,
                                                                                               Se.Set Beam.category (Kernel.Prelude.Just category),
                                                                                               Se.Set Beam.categoryOrder ((categoryMeta >>= (.categoryOrder))),
                                                                                               Se.Set Beam.code ((categoryMeta <&> (.code))),
                                                                                               Se.Set Beam.description ((categoryMeta <&> (.description))),
                                                                                               Se.Set Beam.title ((categoryMeta <&> (.title))),
                                                                                               Se.Set Beam.tnc ((categoryMeta <&> (.tnc))),
                                                                                               Se.Set Beam.finalPrice ((Kernel.Prelude.fmap (.amount)) finalPrice),
                                                                                               Se.Set Beam.holdId holdId,
                                                                                               Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
                                                                                               Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
                                                                                               Se.Set Beam.offeredPrice ((.amount) offeredPrice),
                                                                                               Se.Set Beam.currency (((Kernel.Prelude.Just . (.currency))) price),
                                                                                               Se.Set Beam.price ((.amount) price),
                                                                                               Se.Set Beam.quoteId (Kernel.Types.Id.getId quoteId),
                                                                                               Se.Set Beam.seatIds ((fmap Kernel.Types.Id.getId) <$> seatIds),
                                                                                               Se.Set Beam.seatLabels seatLabels,
                                                                                               Se.Set Beam.selectedQuantity (Kernel.Prelude.Just selectedQuantity),
                                                                                               Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



