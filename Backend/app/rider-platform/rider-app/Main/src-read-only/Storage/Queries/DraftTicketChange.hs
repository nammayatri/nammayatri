{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DraftTicketChange where

import qualified Data.Aeson
import qualified Domain.Types.DraftTicketChange
import qualified Domain.Types.EventManagement
import qualified Domain.Types.TicketPlace
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DraftTicketChange as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DraftTicketChange.DraftTicketChange -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DraftTicketChange.DraftTicketChange] -> m ())
createMany = traverse_ create

deleteDraftById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> m ())
deleteDraftById id = do deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findAllByStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Maybe Int -> Maybe Int -> Domain.Types.DraftTicketChange.DraftStatus -> m [Domain.Types.DraftTicketChange.DraftTicketChange])
findAllByStatus limit offset status = do findAllWithOptionsKV [Se.Is Beam.status $ Se.Eq status] (Se.Desc Beam.createdAt) limit offset

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> m (Maybe Domain.Types.DraftTicketChange.DraftTicketChange))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateDraftById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Domain.Types.EventManagement.TicketPlaceDef -> Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> m ())
updateDraftById draftPayload isApprovalRequired id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.draftPayload (Data.Aeson.toJSON <$> draftPayload),
      Se.Set Beam.isApprovalRequired isApprovalRequired,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateMessage :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> m ())
updateMessage message id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.message message, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DraftTicketChange.DraftStatus -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> m ())
updateStatus status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> m (Maybe Domain.Types.DraftTicketChange.DraftTicketChange))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DraftTicketChange.DraftTicketChange -> m ())
updateByPrimaryKey (Domain.Types.DraftTicketChange.DraftTicketChange {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.draftPayload (Data.Aeson.toJSON <$> draftPayload),
      Se.Set Beam.isApprovalRequired isApprovalRequired,
      Se.Set Beam.message message,
      Se.Set Beam.status status,
      Se.Set Beam.ticketMerchantId ticketMerchantId,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.DraftTicketChange Domain.Types.DraftTicketChange.DraftTicketChange where
  fromTType' (Beam.DraftTicketChangeT {..}) = do
    pure $
      Just
        Domain.Types.DraftTicketChange.DraftTicketChange
          { createdAt = createdAt,
            draftPayload = (\val -> case Data.Aeson.fromJSON val of Data.Aeson.Success x -> Just x; Data.Aeson.Error _ -> Nothing) =<< draftPayload,
            id = Kernel.Types.Id.Id id,
            isApprovalRequired = isApprovalRequired,
            message = message,
            status = status,
            ticketMerchantId = ticketMerchantId,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.DraftTicketChange Domain.Types.DraftTicketChange.DraftTicketChange where
  toTType' (Domain.Types.DraftTicketChange.DraftTicketChange {..}) = do
    Beam.DraftTicketChangeT
      { Beam.createdAt = createdAt,
        Beam.draftPayload = Data.Aeson.toJSON <$> draftPayload,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isApprovalRequired = isApprovalRequired,
        Beam.message = message,
        Beam.status = status,
        Beam.ticketMerchantId = ticketMerchantId,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
