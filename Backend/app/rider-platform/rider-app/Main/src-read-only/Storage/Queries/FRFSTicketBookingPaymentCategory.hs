{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSTicketBookingPaymentCategory where

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
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSTicketBookingPaymentCategory as Beam
import qualified Storage.Queries.Transformers.FRFSQuoteCategory

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSTicketBookingPaymentCategory.FRFSTicketBookingPaymentCategory -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSTicketBookingPaymentCategory.FRFSTicketBookingPaymentCategory] -> m ())
createMany = traverse_ create

findAllByPaymentId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment -> m [Domain.Types.FRFSTicketBookingPaymentCategory.FRFSTicketBookingPaymentCategory])
findAllByPaymentId frfsTicketBookingPaymentId = do findAllWithKV [Se.Is Beam.frfsTicketBookingPaymentId $ Se.Eq (Kernel.Types.Id.getId frfsTicketBookingPaymentId)]

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FRFSTicketBookingPaymentCategory.FRFSTicketBookingPaymentCategory -> m (Maybe Domain.Types.FRFSTicketBookingPaymentCategory.FRFSTicketBookingPaymentCategory))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FRFSTicketBookingPaymentCategory.FRFSTicketBookingPaymentCategory -> m (Maybe Domain.Types.FRFSTicketBookingPaymentCategory.FRFSTicketBookingPaymentCategory))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSTicketBookingPaymentCategory.FRFSTicketBookingPaymentCategory -> m ())
updateByPrimaryKey (Domain.Types.FRFSTicketBookingPaymentCategory.FRFSTicketBookingPaymentCategory {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bppItemId bppItemId,
      Se.Set Beam.category (Kernel.Prelude.Just category),
      Se.Set Beam.code (categoryMeta <&> (.code)),
      Se.Set Beam.description (categoryMeta <&> (.description)),
      Se.Set Beam.title (categoryMeta <&> (.title)),
      Se.Set Beam.tnc (categoryMeta <&> (.tnc)),
      Se.Set Beam.finalPrice (Kernel.Prelude.fmap (.amount) finalPrice),
      Se.Set Beam.frfsTicketBookingPaymentId (Kernel.Types.Id.getId frfsTicketBookingPaymentId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.offeredPrice ((.amount) offeredPrice),
      Se.Set Beam.currency ((Kernel.Prelude.Just . (.currency)) price),
      Se.Set Beam.price ((.amount) price),
      Se.Set Beam.quoteId (Kernel.Types.Id.getId quoteId),
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
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            offeredPrice = Kernel.Types.Common.mkPrice currency offeredPrice,
            price = Kernel.Types.Common.mkPrice currency price,
            quoteId = Kernel.Types.Id.Id quoteId,
            selectedQuantity = Kernel.Prelude.fromMaybe 0 selectedQuantity,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSTicketBookingPaymentCategory Domain.Types.FRFSTicketBookingPaymentCategory.FRFSTicketBookingPaymentCategory where
  toTType' (Domain.Types.FRFSTicketBookingPaymentCategory.FRFSTicketBookingPaymentCategory {..}) = do
    Beam.FRFSTicketBookingPaymentCategoryT
      { Beam.bppItemId = bppItemId,
        Beam.category = Kernel.Prelude.Just category,
        Beam.code = categoryMeta <&> (.code),
        Beam.description = categoryMeta <&> (.description),
        Beam.title = categoryMeta <&> (.title),
        Beam.tnc = categoryMeta <&> (.tnc),
        Beam.finalPrice = Kernel.Prelude.fmap (.amount) finalPrice,
        Beam.frfsTicketBookingPaymentId = Kernel.Types.Id.getId frfsTicketBookingPaymentId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.offeredPrice = (.amount) offeredPrice,
        Beam.currency = (Kernel.Prelude.Just . (.currency)) price,
        Beam.price = (.amount) price,
        Beam.quoteId = Kernel.Types.Id.getId quoteId,
        Beam.selectedQuantity = Kernel.Prelude.Just selectedQuantity,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
