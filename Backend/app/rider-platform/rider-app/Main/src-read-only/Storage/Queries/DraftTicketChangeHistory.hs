{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DraftTicketChangeHistory where

import qualified Data.Aeson
import qualified Domain.Types.DraftTicketChange
import qualified Domain.Types.DraftTicketChangeHistory
import qualified Domain.Types.TicketPlace
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DraftTicketChangeHistory as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DraftTicketChangeHistory.DraftTicketChangeHistory -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DraftTicketChangeHistory.DraftTicketChangeHistory] -> m ())
createMany = traverse_ create

findByTicketPlaceId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> m [Domain.Types.DraftTicketChangeHistory.DraftTicketChangeHistory])
findByTicketPlaceId limit offset ticketPlaceId = do findAllWithOptionsKV [Se.Is Beam.ticketPlaceId $ Se.Eq (Kernel.Types.Id.getId ticketPlaceId)] (Se.Desc Beam.createdAt) limit offset

findByTicketPlaceIdAndStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Domain.Types.DraftTicketChange.DraftStatus -> m [Domain.Types.DraftTicketChangeHistory.DraftTicketChangeHistory])
findByTicketPlaceIdAndStatus limit offset ticketPlaceId status = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.ticketPlaceId $ Se.Eq (Kernel.Types.Id.getId ticketPlaceId),
          Se.Is Beam.status $ Se.Eq status
        ]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.DraftTicketChangeHistory.DraftTicketChangeHistory -> m (Maybe Domain.Types.DraftTicketChangeHistory.DraftTicketChangeHistory))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DraftTicketChangeHistory.DraftTicketChangeHistory -> m ())
updateByPrimaryKey (Domain.Types.DraftTicketChangeHistory.DraftTicketChangeHistory {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.draftPayload (Data.Aeson.toJSON <$> draftPayload),
      Se.Set Beam.isApprovalRequired isApprovalRequired,
      Se.Set Beam.message message,
      Se.Set Beam.reviewedBy reviewedBy,
      Se.Set Beam.status status,
      Se.Set Beam.ticketMerchantId ticketMerchantId,
      Se.Set Beam.ticketPlaceId (Kernel.Types.Id.getId ticketPlaceId),
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.DraftTicketChangeHistory Domain.Types.DraftTicketChangeHistory.DraftTicketChangeHistory where
  fromTType' (Beam.DraftTicketChangeHistoryT {..}) = do
    pure $
      Just
        Domain.Types.DraftTicketChangeHistory.DraftTicketChangeHistory
          { createdAt = createdAt,
            draftPayload = (\val -> case Data.Aeson.fromJSON val of Data.Aeson.Success x -> Just x; Data.Aeson.Error _ -> Nothing) =<< draftPayload,
            id = Kernel.Types.Id.Id id,
            isApprovalRequired = isApprovalRequired,
            message = message,
            reviewedBy = reviewedBy,
            status = status,
            ticketMerchantId = ticketMerchantId,
            ticketPlaceId = Kernel.Types.Id.Id ticketPlaceId,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.DraftTicketChangeHistory Domain.Types.DraftTicketChangeHistory.DraftTicketChangeHistory where
  toTType' (Domain.Types.DraftTicketChangeHistory.DraftTicketChangeHistory {..}) = do
    Beam.DraftTicketChangeHistoryT
      { Beam.createdAt = createdAt,
        Beam.draftPayload = Data.Aeson.toJSON <$> draftPayload,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isApprovalRequired = isApprovalRequired,
        Beam.message = message,
        Beam.reviewedBy = reviewedBy,
        Beam.status = status,
        Beam.ticketMerchantId = ticketMerchantId,
        Beam.ticketPlaceId = Kernel.Types.Id.getId ticketPlaceId,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
