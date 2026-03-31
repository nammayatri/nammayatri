{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.Offer where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.Offer
import qualified Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.Offer as Beam
import qualified Sequelize as Se

create :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.Offer.Offer -> m ())
create = createWithKV

createMany :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Payment.Domain.Types.Offer.Offer] -> m ())
createMany = traverse_ create

findAllActiveByMerchant :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Bool -> m ([Lib.Payment.Domain.Types.Offer.Offer]))
findAllActiveByMerchant merchantId merchantOperatingCityId isActive = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId,
          Se.Is Beam.isActive $ Se.Eq isActive
        ]
    ]

findById :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Payment.Domain.Types.Offer.Offer -> m (Maybe Lib.Payment.Domain.Types.Offer.Offer))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByOfferCode :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m (Maybe Lib.Payment.Domain.Types.Offer.Offer))
findByOfferCode offerCode = do findOneWithKV [Se.Is Beam.offerCode $ Se.Eq offerCode]

findByPrimaryKey :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Payment.Domain.Types.Offer.Offer -> m (Maybe Lib.Payment.Domain.Types.Offer.Offer))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.Offer.Offer -> m ())
updateByPrimaryKey (Lib.Payment.Domain.Types.Offer.Offer {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.currency currency,
      Se.Set Beam.description description,
      Se.Set Beam.discountType discountType,
      Se.Set Beam.discountValue discountValue,
      Se.Set Beam.isActive isActive,
      Se.Set Beam.maxDiscount maxDiscount,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.offerCode offerCode,
      Se.Set Beam.offerEligibilityJsonLogic offerEligibilityJsonLogic,
      Se.Set Beam.offerType offerType,
      Se.Set Beam.sponsoredBy sponsoredBy,
      Se.Set Beam.title title,
      Se.Set Beam.tnc tnc,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.Offer Lib.Payment.Domain.Types.Offer.Offer where
  fromTType' (Beam.OfferT {..}) = do
    pure $
      Just
        Lib.Payment.Domain.Types.Offer.Offer
          { createdAt = createdAt,
            currency = currency,
            description = description,
            discountType = discountType,
            discountValue = discountValue,
            id = Kernel.Types.Id.Id id,
            isActive = isActive,
            maxDiscount = maxDiscount,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            offerCode = offerCode,
            offerEligibilityJsonLogic = offerEligibilityJsonLogic,
            offerType = offerType,
            sponsoredBy = sponsoredBy,
            title = title,
            tnc = tnc,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Offer Lib.Payment.Domain.Types.Offer.Offer where
  toTType' (Lib.Payment.Domain.Types.Offer.Offer {..}) = do
    Beam.OfferT
      { Beam.createdAt = createdAt,
        Beam.currency = currency,
        Beam.description = description,
        Beam.discountType = discountType,
        Beam.discountValue = discountValue,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isActive = isActive,
        Beam.maxDiscount = maxDiscount,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.offerCode = offerCode,
        Beam.offerEligibilityJsonLogic = offerEligibilityJsonLogic,
        Beam.offerType = offerType,
        Beam.sponsoredBy = sponsoredBy,
        Beam.title = title,
        Beam.tnc = tnc,
        Beam.updatedAt = updatedAt
      }
