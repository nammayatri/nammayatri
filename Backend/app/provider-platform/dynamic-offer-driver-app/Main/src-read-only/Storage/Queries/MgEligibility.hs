{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MgEligibility (module Storage.Queries.MgEligibility, module ReExport) where

import qualified Domain.Types.MgCampaign
import qualified Domain.Types.MgEligibility
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MgEligibility as Beam
import Storage.Queries.MgEligibilityExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MgEligibility.MgEligibility -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MgEligibility.MgEligibility] -> m ())
createMany = traverse_ create

findAllByCampaignId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MgCampaign.MgCampaign -> m [Domain.Types.MgEligibility.MgEligibility])
findAllByCampaignId campaignId = do findAllWithKV [Se.Is Beam.campaignId $ Se.Eq (Kernel.Types.Id.getId campaignId)]

findByCampaignIdAndDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MgCampaign.MgCampaign -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.MgEligibility.MgEligibility))
findByCampaignIdAndDriverId campaignId driverId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.campaignId $ Se.Eq (Kernel.Types.Id.getId campaignId),
          Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)
        ]
    ]

updateNotifiedAtById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.MgEligibility.MgEligibility -> m ())
updateNotifiedAtById notifiedAt id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.notifiedAt notifiedAt, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MgEligibility.MgEligibility -> m (Maybe Domain.Types.MgEligibility.MgEligibility))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MgEligibility.MgEligibility -> m ())
updateByPrimaryKey (Domain.Types.MgEligibility.MgEligibility {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.campaignId (Kernel.Types.Id.getId campaignId),
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.invitedAt invitedAt,
      Se.Set Beam.notifiedAt notifiedAt,
      Se.Set Beam.reason reason,
      Se.Set Beam.source source,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
