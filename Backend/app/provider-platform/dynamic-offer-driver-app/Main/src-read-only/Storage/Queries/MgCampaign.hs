{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MgCampaign (module Storage.Queries.MgCampaign, module ReExport) where

import qualified Data.Time.Calendar
import qualified Domain.Types.MgCampaign
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Text
import qualified Sequelize as Se
import qualified Storage.Beam.MgCampaign as Beam
import Storage.Queries.MgCampaignExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MgCampaign.MgCampaign -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MgCampaign.MgCampaign] -> m ())
createMany = traverse_ create

updateEndDateAndMaxBudgetById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Data.Time.Calendar.Day -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Domain.Types.MgCampaign.MgCampaign -> m ())
updateEndDateAndMaxBudgetById endDate maxBudget id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.endDate endDate, Se.Set Beam.maxBudget maxBudget, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatusById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MgCampaign.MgCampaignStatus -> Kernel.Types.Id.Id Domain.Types.MgCampaign.MgCampaign -> m ())
updateStatusById status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MgCampaign.MgCampaign -> m (Maybe Domain.Types.MgCampaign.MgCampaign))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MgCampaign.MgCampaign -> m ())
updateByPrimaryKey (Domain.Types.MgCampaign.MgCampaign {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.createdBy createdBy,
      Se.Set Beam.currency currency,
      Se.Set Beam.description description,
      Se.Set Beam.endDate endDate,
      Se.Set Beam.forceAssign forceAssign,
      Se.Set Beam.frequency frequency,
      Se.Set Beam.guaranteeAmount guaranteeAmount,
      Se.Set Beam.maxBudget maxBudget,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.name name,
      Se.Set Beam.onFleetChange onFleetChange,
      Se.Set Beam.optInCutoffMinutes optInCutoffMinutes,
      Se.Set Beam.settlementGraceMinutes settlementGraceMinutes,
      Se.Set Beam.startDate startDate,
      Se.Set Beam.status status,
      Se.Set Beam.vehicleCategories (Kernel.Utils.Text.encodeToText vehicleCategories),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
