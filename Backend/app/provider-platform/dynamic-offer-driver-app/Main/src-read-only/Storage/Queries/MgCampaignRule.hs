{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MgCampaignRule where

import qualified Domain.Types.MgCampaign
import qualified Domain.Types.MgCampaignRule
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Text
import qualified Sequelize as Se
import qualified Storage.Beam.MgCampaignRule as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MgCampaignRule.MgCampaignRule -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MgCampaignRule.MgCampaignRule] -> m ())
createMany = traverse_ create

deleteByCampaignId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MgCampaign.MgCampaign -> m ())
deleteByCampaignId campaignId = do deleteWithKV [Se.Is Beam.campaignId $ Se.Eq (Kernel.Types.Id.getId campaignId)]

findAllByCampaignId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MgCampaign.MgCampaign -> m [Domain.Types.MgCampaignRule.MgCampaignRule])
findAllByCampaignId campaignId = do findAllWithKV [Se.Is Beam.campaignId $ Se.Eq (Kernel.Types.Id.getId campaignId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MgCampaignRule.MgCampaignRule -> m (Maybe Domain.Types.MgCampaignRule.MgCampaignRule))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MgCampaignRule.MgCampaignRule -> m ())
updateByPrimaryKey (Domain.Types.MgCampaignRule.MgCampaignRule {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.campaignId (Kernel.Types.Id.getId campaignId),
      Se.Set Beam.metric metric,
      Se.Set Beam.params (Kernel.Utils.Text.encodeToText <$> params),
      Se.Set Beam.slabs (Kernel.Utils.Text.encodeToText slabs),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.MgCampaignRule Domain.Types.MgCampaignRule.MgCampaignRule where
  fromTType' (Beam.MgCampaignRuleT {..}) = do
    pure $
      Just
        Domain.Types.MgCampaignRule.MgCampaignRule
          { campaignId = Kernel.Types.Id.Id campaignId,
            id = Kernel.Types.Id.Id id,
            metric = metric,
            params = params >>= Kernel.Utils.Text.decodeFromText,
            slabs = fromMaybe [] (Kernel.Utils.Text.decodeFromText slabs),
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.MgCampaignRule Domain.Types.MgCampaignRule.MgCampaignRule where
  toTType' (Domain.Types.MgCampaignRule.MgCampaignRule {..}) = do
    Beam.MgCampaignRuleT
      { Beam.campaignId = Kernel.Types.Id.getId campaignId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.metric = metric,
        Beam.params = Kernel.Utils.Text.encodeToText <$> params,
        Beam.slabs = Kernel.Utils.Text.encodeToText slabs,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
