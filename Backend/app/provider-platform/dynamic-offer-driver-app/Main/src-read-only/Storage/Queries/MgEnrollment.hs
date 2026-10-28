{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MgEnrollment (module Storage.Queries.MgEnrollment, module ReExport) where

import qualified Data.Time.Calendar
import qualified Domain.Types.MgCampaign
import qualified Domain.Types.MgEnrollment
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Text
import qualified Sequelize as Se
import qualified Storage.Beam.MgEnrollment as Beam
import Storage.Queries.MgEnrollmentExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MgEnrollment.MgEnrollment -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MgEnrollment.MgEnrollment] -> m ())
createMany = traverse_ create

findAllByCampaignIdAndPeriodDate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MgCampaign.MgCampaign -> Data.Time.Calendar.Day -> m [Domain.Types.MgEnrollment.MgEnrollment])
findAllByCampaignIdAndPeriodDate campaignId periodDate = do findAllWithKV [Se.And [Se.Is Beam.campaignId $ Se.Eq (Kernel.Types.Id.getId campaignId), Se.Is Beam.periodDate $ Se.Eq periodDate]]

findAllByDriverIdAndPeriodDate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Data.Time.Calendar.Day -> m [Domain.Types.MgEnrollment.MgEnrollment])
findAllByDriverIdAndPeriodDate driverId periodDate = do findAllWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId), Se.Is Beam.periodDate $ Se.Eq periodDate]]

findByCampaignIdDriverIdAndPeriodDate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MgCampaign.MgCampaign -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Data.Time.Calendar.Day -> m (Maybe Domain.Types.MgEnrollment.MgEnrollment))
findByCampaignIdDriverIdAndPeriodDate campaignId driverId periodDate = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.campaignId $ Se.Eq (Kernel.Types.Id.getId campaignId),
          Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.periodDate $ Se.Eq periodDate
        ]
    ]

updateStatusById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MgEnrollment.MgEnrollmentStatus -> Kernel.Types.Id.Id Domain.Types.MgEnrollment.MgEnrollment -> m ())
updateStatusById status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MgEnrollment.MgEnrollment -> m (Maybe Domain.Types.MgEnrollment.MgEnrollment))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MgEnrollment.MgEnrollment -> m ())
updateByPrimaryKey (Domain.Types.MgEnrollment.MgEnrollment {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.beneficiaryId beneficiaryId,
      Se.Set Beam.beneficiaryType beneficiaryType,
      Se.Set Beam.campaignId (Kernel.Types.Id.getId campaignId),
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.events (Kernel.Utils.Text.encodeToText events),
      Se.Set Beam.ledgerEntryId ledgerEntryId,
      Se.Set Beam.metrics (Kernel.Utils.Text.encodeToText metrics),
      Se.Set Beam.optedInAt optedInAt,
      Se.Set Beam.optedInBy (Kernel.Types.Id.getId optedInBy),
      Se.Set Beam.payoutPercent payoutPercent,
      Se.Set Beam.periodDate periodDate,
      Se.Set Beam.settledAt settledAt,
      Se.Set Beam.status status,
      Se.Set Beam.termsAcceptedAt termsAcceptedAt,
      Se.Set Beam.topUpAmount topUpAmount,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
