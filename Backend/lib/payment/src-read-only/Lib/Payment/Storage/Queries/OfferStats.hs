{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Lib.Payment.Storage.Queries.OfferStats where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Lib.Payment.Domain.Types.OfferStats
import qualified Lib.Payment.Storage.Beam.OfferStats as Beam
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.Offer
import qualified Lib.Payment.Storage.Beam.BeamFlow
import qualified Sequelize as Se



create :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.OfferStats.OfferStats -> m ())
create = createWithKV
createMany :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Payment.Domain.Types.OfferStats.OfferStats] -> m ())
createMany = traverse_ create
findAllByEntityIdAndEntityType :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
                                  (Kernel.Prelude.Text -> Lib.Payment.Domain.Types.OfferStats.OfferStatsEntityType -> m ([Lib.Payment.Domain.Types.OfferStats.OfferStats]))
findAllByEntityIdAndEntityType entityId entityType = do findAllWithKV [Se.And [Se.Is Beam.personId $ Se.Eq entityId, Se.Is Beam.entityType $ Se.Eq (Kernel.Prelude.Just entityType)]]
findByOfferIdAndEntityIdAndEntityType :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
                                         (Kernel.Types.Id.Id Lib.Payment.Domain.Types.Offer.Offer -> Kernel.Prelude.Text -> Lib.Payment.Domain.Types.OfferStats.OfferStatsEntityType -> m (Maybe Lib.Payment.Domain.Types.OfferStats.OfferStats))
findByOfferIdAndEntityIdAndEntityType offerId entityId entityType = do findOneWithKV [Se.And [Se.Is Beam.offerId $ Se.Eq (Kernel.Types.Id.getId offerId),
                                                                                              Se.Is Beam.personId $ Se.Eq entityId,
                                                                                              Se.Is Beam.entityType $ Se.Eq (Kernel.Prelude.Just entityType)]]
findByPrimaryKey :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Payment.Domain.Types.OfferStats.OfferStats -> m (Maybe Lib.Payment.Domain.Types.OfferStats.OfferStats))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.OfferStats.OfferStats -> m ())
updateByPrimaryKey (Lib.Payment.Domain.Types.OfferStats.OfferStats {..}) = do {_now <- getCurrentTime;
                                                                               updateWithKV [Se.Set Beam.personId entityId,
                                                                                             Se.Set Beam.entityType (Kernel.Prelude.Just entityType),
                                                                                             Se.Set Beam.offerAppliedCount offerAppliedCount,
                                                                                             Se.Set Beam.offerId (Kernel.Types.Id.getId offerId),
                                                                                             Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



instance FromTType' Beam.OfferStats Lib.Payment.Domain.Types.OfferStats.OfferStats
    where fromTType' (Beam.OfferStatsT {..}) = do pure $ Just Lib.Payment.Domain.Types.OfferStats.OfferStats{createdAt = createdAt,
                                                                                                             entityId = personId,
                                                                                                             entityType = Kernel.Prelude.fromMaybe Lib.Payment.Domain.Types.OfferStats.Person entityType,
                                                                                                             id = Kernel.Types.Id.Id id,
                                                                                                             offerAppliedCount = offerAppliedCount,
                                                                                                             offerId = Kernel.Types.Id.Id offerId,
                                                                                                             updatedAt = updatedAt}
instance ToTType' Beam.OfferStats Lib.Payment.Domain.Types.OfferStats.OfferStats
    where toTType' (Lib.Payment.Domain.Types.OfferStats.OfferStats {..}) = do Beam.OfferStatsT{Beam.createdAt = createdAt,
                                                                                               Beam.personId = entityId,
                                                                                               Beam.entityType = Kernel.Prelude.Just entityType,
                                                                                               Beam.id = Kernel.Types.Id.getId id,
                                                                                               Beam.offerAppliedCount = offerAppliedCount,
                                                                                               Beam.offerId = Kernel.Types.Id.getId offerId,
                                                                                               Beam.updatedAt = updatedAt}



