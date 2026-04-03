{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Lib.Payment.Storage.Queries.OfflineOffer where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Lib.Payment.Domain.Types.OfflineOffer
import qualified Lib.Payment.Storage.Beam.OfflineOffer as Beam
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.Payment.Storage.Beam.BeamFlow
import qualified Sequelize as Se



create :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.OfflineOffer.OfflineOffer -> m ())
create = createWithKV
createMany :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Payment.Domain.Types.OfflineOffer.OfflineOffer] -> m ())
createMany = traverse_ create
findById :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Payment.Domain.Types.OfflineOffer.OfflineOffer -> m (Maybe Lib.Payment.Domain.Types.OfflineOffer.OfflineOffer))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
findByReferenceId :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m ([Lib.Payment.Domain.Types.OfflineOffer.OfflineOffer]))
findByReferenceId referenceId = do findAllWithKV [Se.Is Beam.referenceId $ Se.Eq referenceId]
findByPrimaryKey :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
                    (Kernel.Types.Id.Id Lib.Payment.Domain.Types.OfflineOffer.OfflineOffer -> m (Maybe Lib.Payment.Domain.Types.OfflineOffer.OfflineOffer))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.OfflineOffer.OfflineOffer -> m ())
updateByPrimaryKey (Lib.Payment.Domain.Types.OfflineOffer.OfflineOffer {..}) = do {_now <- getCurrentTime;
                                                                                   updateWithKV [Se.Set Beam.discountAmount discountAmount,
                                                                                                 Se.Set Beam.merchantId merchantId,
                                                                                                 Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
                                                                                                 Se.Set Beam.offerCode offerCode,
                                                                                                 Se.Set Beam.offerId offerId,
                                                                                                 Se.Set Beam.payoutAmount payoutAmount,
                                                                                                 Se.Set Beam.referenceId referenceId,
                                                                                                 Se.Set Beam.status status,
                                                                                                 Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



instance FromTType' Beam.OfflineOffer Lib.Payment.Domain.Types.OfflineOffer.OfflineOffer
    where fromTType' (Beam.OfflineOfferT {..}) = do pure $ Just Lib.Payment.Domain.Types.OfflineOffer.OfflineOffer{createdAt = createdAt,
                                                                                                                   discountAmount = discountAmount,
                                                                                                                   id = Kernel.Types.Id.Id id,
                                                                                                                   merchantId = merchantId,
                                                                                                                   merchantOperatingCityId = merchantOperatingCityId,
                                                                                                                   offerCode = offerCode,
                                                                                                                   offerId = offerId,
                                                                                                                   payoutAmount = payoutAmount,
                                                                                                                   referenceId = referenceId,
                                                                                                                   status = status,
                                                                                                                   updatedAt = updatedAt}
instance ToTType' Beam.OfflineOffer Lib.Payment.Domain.Types.OfflineOffer.OfflineOffer
    where toTType' (Lib.Payment.Domain.Types.OfflineOffer.OfflineOffer {..}) = do Beam.OfflineOfferT{Beam.createdAt = createdAt,
                                                                                                     Beam.discountAmount = discountAmount,
                                                                                                     Beam.id = Kernel.Types.Id.getId id,
                                                                                                     Beam.merchantId = merchantId,
                                                                                                     Beam.merchantOperatingCityId = merchantOperatingCityId,
                                                                                                     Beam.offerCode = offerCode,
                                                                                                     Beam.offerId = offerId,
                                                                                                     Beam.payoutAmount = payoutAmount,
                                                                                                     Beam.referenceId = referenceId,
                                                                                                     Beam.status = status,
                                                                                                     Beam.updatedAt = updatedAt}



