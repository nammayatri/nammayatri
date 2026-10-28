{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MgCampaignWindow where

import qualified Domain.Types.MgCampaign
import qualified Domain.Types.MgCampaignWindow
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MgCampaignWindow as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MgCampaignWindow.MgCampaignWindow -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MgCampaignWindow.MgCampaignWindow] -> m ())
createMany = traverse_ create

deleteByCampaignId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MgCampaign.MgCampaign -> m ())
deleteByCampaignId campaignId = do deleteWithKV [Se.Is Beam.campaignId $ Se.Eq (Kernel.Types.Id.getId campaignId)]

findAllByCampaignId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MgCampaign.MgCampaign -> m [Domain.Types.MgCampaignWindow.MgCampaignWindow])
findAllByCampaignId campaignId = do findAllWithKV [Se.Is Beam.campaignId $ Se.Eq (Kernel.Types.Id.getId campaignId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MgCampaignWindow.MgCampaignWindow -> m (Maybe Domain.Types.MgCampaignWindow.MgCampaignWindow))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MgCampaignWindow.MgCampaignWindow -> m ())
updateByPrimaryKey (Domain.Types.MgCampaignWindow.MgCampaignWindow {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.campaignId (Kernel.Types.Id.getId campaignId),
      Se.Set Beam.endTime endTime,
      Se.Set Beam.startTime startTime,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.MgCampaignWindow Domain.Types.MgCampaignWindow.MgCampaignWindow where
  fromTType' (Beam.MgCampaignWindowT {..}) = do
    pure $
      Just
        Domain.Types.MgCampaignWindow.MgCampaignWindow
          { campaignId = Kernel.Types.Id.Id campaignId,
            endTime = endTime,
            id = Kernel.Types.Id.Id id,
            startTime = startTime,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.MgCampaignWindow Domain.Types.MgCampaignWindow.MgCampaignWindow where
  toTType' (Domain.Types.MgCampaignWindow.MgCampaignWindow {..}) = do
    Beam.MgCampaignWindowT
      { Beam.campaignId = Kernel.Types.Id.getId campaignId,
        Beam.endTime = endTime,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.startTime = startTime,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
