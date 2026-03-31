{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.PersonOfferStats where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.Offer
import qualified Lib.Payment.Domain.Types.PersonOfferStats
import qualified Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.PersonOfferStats as Beam
import qualified Sequelize as Se

create :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.PersonOfferStats.PersonOfferStats -> m ())
create = createWithKV

createMany :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Payment.Domain.Types.PersonOfferStats.PersonOfferStats] -> m ())
createMany = traverse_ create

findAllByPersonId :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m ([Lib.Payment.Domain.Types.PersonOfferStats.PersonOfferStats]))
findAllByPersonId personId = do findAllWithKV [Se.Is Beam.personId $ Se.Eq personId]

findByOfferIdAndPersonId ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.Offer.Offer -> Kernel.Prelude.Text -> m (Maybe Lib.Payment.Domain.Types.PersonOfferStats.PersonOfferStats))
findByOfferIdAndPersonId offerId personId = do findOneWithKV [Se.And [Se.Is Beam.offerId $ Se.Eq (Kernel.Types.Id.getId offerId), Se.Is Beam.personId $ Se.Eq personId]]

findByPrimaryKey ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PersonOfferStats.PersonOfferStats -> m (Maybe Lib.Payment.Domain.Types.PersonOfferStats.PersonOfferStats))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.PersonOfferStats.PersonOfferStats -> m ())
updateByPrimaryKey (Lib.Payment.Domain.Types.PersonOfferStats.PersonOfferStats {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.offerAppliedCount offerAppliedCount,
      Se.Set Beam.offerId (Kernel.Types.Id.getId offerId),
      Se.Set Beam.personId personId,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.PersonOfferStats Lib.Payment.Domain.Types.PersonOfferStats.PersonOfferStats where
  fromTType' (Beam.PersonOfferStatsT {..}) = do
    pure $
      Just
        Lib.Payment.Domain.Types.PersonOfferStats.PersonOfferStats
          { createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            offerAppliedCount = offerAppliedCount,
            offerId = Kernel.Types.Id.Id offerId,
            personId = personId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PersonOfferStats Lib.Payment.Domain.Types.PersonOfferStats.PersonOfferStats where
  toTType' (Lib.Payment.Domain.Types.PersonOfferStats.PersonOfferStats {..}) = do
    Beam.PersonOfferStatsT
      { Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.offerAppliedCount = offerAppliedCount,
        Beam.offerId = Kernel.Types.Id.getId offerId,
        Beam.personId = personId,
        Beam.updatedAt = updatedAt
      }
