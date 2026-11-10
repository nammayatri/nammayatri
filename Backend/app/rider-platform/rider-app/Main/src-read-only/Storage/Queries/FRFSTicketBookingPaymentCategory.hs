{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSTicketBookingPaymentCategory where

import qualified Domain.Types.FRFSQuoteCategory
import qualified Domain.Types.FRFSQuoteCategoryType
import qualified Domain.Types.FRFSTicketBookingPayment
import qualified Domain.Types.FRFSTicketBookingPaymentCategory
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import qualified Kernel.Types.Price
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSTicketBookingPaymentCategory as Beam
import qualified Storage.Queries.Transformers.FRFSQuoteCategory

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.FRFSTicketBookingPaymentCategory.FRFSTicketBookingPaymentCategory -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.FRFSTicketBookingPaymentCategory.FRFSTicketBookingPaymentCategory] -> m ()
createMany = traverse_ create

findAllByPaymentId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment ->
  m [Domain.Types.FRFSTicketBookingPaymentCategory.FRFSTicketBookingPaymentCategory]
findAllByPaymentId frfsTicketBookingPaymentId = do findAllWithKV [Se.Is Beam.frfsTicketBookingPaymentId $ Se.Eq (Kernel.Types.Id.getId frfsTicketBookingPaymentId)]

findAllByPaymentIds ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  [Kernel.Types.Id.Id Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment] ->
  m [Domain.Types.FRFSTicketBookingPaymentCategory.FRFSTicketBookingPaymentCategory]
findAllByPaymentIds frfsTicketBookingPaymentId = do findAllWithKV [Se.Is Beam.frfsTicketBookingPaymentId $ Se.In (Kernel.Types.Id.getId <$> frfsTicketBookingPaymentId)]

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.FRFSTicketBookingPaymentCategory.FRFSTicketBookingPaymentCategory ->
  m (Maybe Domain.Types.FRFSTicketBookingPaymentCategory.FRFSTicketBookingPaymentCategory)
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.FRFSTicketBookingPaymentCategory.FRFSTicketBookingPaymentCategory ->
  m (Maybe Domain.Types.FRFSTicketBookingPaymentCategory.FRFSTicketBookingPaymentCategory)
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.FRFSTicketBookingPaymentCategory.FRFSTicketBookingPaymentCategory -> m ()
updateByPrimaryKey (Domain.Types.FRFSTicketBookingPaymentCategory.FRFSTicketBookingPaymentCategory {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bppItemId bppItemId,
      Se.Set Beam.category (Kernel.Prelude.Just category),
      Se.Set Beam.code (categoryMeta <&> Domain.Types.FRFSQuoteCategory.code),
      Se.Set Beam.description (categoryMeta <&> Domain.Types.FRFSQuoteCategory.description),
      Se.Set Beam.title (categoryMeta <&> Domain.Types.FRFSQuoteCategory.title),
      Se.Set Beam.tnc (categoryMeta <&> Domain.Types.FRFSQuoteCategory.tnc),
      Se.Set Beam.finalPrice (Kernel.Prelude.fmap Kernel.Types.Price.getPriceAmount finalPrice),
      Se.Set Beam.frfsTicketBookingPaymentId (Kernel.Types.Id.getId frfsTicketBookingPaymentId),
      Se.Set Beam.holdId holdId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.offeredPrice (Kernel.Types.Price.getPriceAmount offeredPrice),
      Se.Set Beam.currency ((Kernel.Prelude.Just . Kernel.Types.Price.getPriceCurrency) price),
      Se.Set Beam.price (Kernel.Types.Price.getPriceAmount price),
      Se.Set Beam.quoteId (Kernel.Types.Id.getId quoteId),
      Se.Set Beam.seatIds (fmap Kernel.Types.Id.getId <$> seatIds),
      Se.Set Beam.seatLabels seatLabels,
      Se.Set Beam.selectedQuantity (Kernel.Prelude.Just selectedQuantity),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FRFSTicketBookingPaymentCategory Domain.Types.FRFSTicketBookingPaymentCategory.FRFSTicketBookingPaymentCategory where
  fromTType' (Beam.FRFSTicketBookingPaymentCategoryT {..}) = do
    pure $
      Just
        Domain.Types.FRFSTicketBookingPaymentCategory.FRFSTicketBookingPaymentCategory
          { bppItemId = bppItemId,
            category = Kernel.Prelude.fromMaybe Domain.Types.FRFSQuoteCategoryType.ADULT category,
            categoryMeta = Storage.Queries.Transformers.FRFSQuoteCategory.mkQuoteCategoryMetadata code title description tnc,
            finalPrice = Kernel.Prelude.fmap (Kernel.Types.Common.mkPrice currency) finalPrice,
            frfsTicketBookingPaymentId = Kernel.Types.Id.Id frfsTicketBookingPaymentId,
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

instance ToTType' Beam.FRFSTicketBookingPaymentCategory Domain.Types.FRFSTicketBookingPaymentCategory.FRFSTicketBookingPaymentCategory where
  toTType' (Domain.Types.FRFSTicketBookingPaymentCategory.FRFSTicketBookingPaymentCategory {..}) = do
    Beam.FRFSTicketBookingPaymentCategoryT
      { Beam.bppItemId = bppItemId,
        Beam.category = Kernel.Prelude.Just category,
        Beam.code = categoryMeta <&> Domain.Types.FRFSQuoteCategory.code,
        Beam.description = categoryMeta <&> Domain.Types.FRFSQuoteCategory.description,
        Beam.title = categoryMeta <&> Domain.Types.FRFSQuoteCategory.title,
        Beam.tnc = categoryMeta <&> Domain.Types.FRFSQuoteCategory.tnc,
        Beam.finalPrice = Kernel.Prelude.fmap Kernel.Types.Price.getPriceAmount finalPrice,
        Beam.frfsTicketBookingPaymentId = Kernel.Types.Id.getId frfsTicketBookingPaymentId,
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
