{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.PaymentOrderOffer where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Lib.Payment.Domain.Types.PaymentOrderOffer
import qualified Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.PaymentOrderOffer as Beam
import qualified Sequelize as Se

create :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.PaymentOrderOffer.PaymentOrderOffer -> m ())
create = createWithKV

createMany :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Payment.Domain.Types.PaymentOrderOffer.PaymentOrderOffer] -> m ())
createMany = traverse_ create

findById ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrderOffer.PaymentOrderOffer -> m (Maybe Lib.Payment.Domain.Types.PaymentOrderOffer.PaymentOrderOffer))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPaymentOrder ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder -> m [Lib.Payment.Domain.Types.PaymentOrderOffer.PaymentOrderOffer])
findByPaymentOrder paymentOrderId = do findAllWithKV [Se.Is Beam.paymentOrderId $ Se.Eq (Kernel.Types.Id.getId paymentOrderId)]

findByPrimaryKey ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrderOffer.PaymentOrderOffer -> m (Maybe Lib.Payment.Domain.Types.PaymentOrderOffer.PaymentOrderOffer))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.PaymentOrderOffer.PaymentOrderOffer -> m ())
updateByPrimaryKey (Lib.Payment.Domain.Types.PaymentOrderOffer.PaymentOrderOffer {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.offer_code offer_code,
      Se.Set Beam.offer_id offer_id,
      Se.Set Beam.paymentOrderId (Kernel.Types.Id.getId paymentOrderId),
      Se.Set Beam.responseJSON responseJSON,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.PaymentOrderOffer Lib.Payment.Domain.Types.PaymentOrderOffer.PaymentOrderOffer where
  fromTType' (Beam.PaymentOrderOfferT {..}) = do
    pure $
      Just
        Lib.Payment.Domain.Types.PaymentOrderOffer.PaymentOrderOffer
          { createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            offer_code = offer_code,
            offer_id = offer_id,
            paymentOrderId = Kernel.Types.Id.Id paymentOrderId,
            responseJSON = responseJSON,
            status = status,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PaymentOrderOffer Lib.Payment.Domain.Types.PaymentOrderOffer.PaymentOrderOffer where
  toTType' (Lib.Payment.Domain.Types.PaymentOrderOffer.PaymentOrderOffer {..}) = do
    Beam.PaymentOrderOfferT
      { Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.offer_code = offer_code,
        Beam.offer_id = offer_id,
        Beam.paymentOrderId = Kernel.Types.Id.getId paymentOrderId,
        Beam.responseJSON = responseJSON,
        Beam.status = status,
        Beam.updatedAt = updatedAt
      }
