{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.FinanceTdsReimbursementRequest (module Lib.Finance.Storage.Queries.FinanceTdsReimbursementRequest, module ReExport) where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.FinanceTdsReimbursementRequest as Beam
import Lib.Finance.Storage.Queries.FinanceTdsReimbursementRequestExtra as ReExport
import qualified Sequelize as Se

create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.FinanceTdsReimbursementRequest -> m ())
create = createWithKV

createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.FinanceTdsReimbursementRequest] -> m ())
createMany = traverse_ create

findAllByFleetOwnerIdQuarterAndAssessmentYear ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Text -> Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.Quarter -> Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.AssessmentYear -> Kernel.Prelude.Text -> m [Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.FinanceTdsReimbursementRequest])
findAllByFleetOwnerIdQuarterAndAssessmentYear fleetOwnerId quarter assessmentYear merchantOperatingCityId = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId,
          Se.Is Beam.quarter $ Se.Eq quarter,
          Se.Is Beam.assessmentYear $ Se.Eq assessmentYear,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId
        ]
    ]

findAllByIds ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  ([Kernel.Types.Id.Id Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.FinanceTdsReimbursementRequest] -> m [Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.FinanceTdsReimbursementRequest])
findAllByIds id = do findAllWithKV [Se.Is Beam.id $ Se.In (Kernel.Types.Id.getId <$> id)]

updateAdminChecker ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.FinanceTdsReimbursementRequest -> m ())
updateAdminChecker adminCheckerId adminCheckerName id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.adminCheckerId adminCheckerId, Se.Set Beam.adminCheckerName adminCheckerName, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateAdminMaker ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.FinanceTdsReimbursementRequest -> m ())
updateAdminMaker adminMakerId adminMakerName id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.adminMakerId adminMakerId, Se.Set Beam.adminMakerName adminMakerName, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatusAndRejectionReason ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.FinanceTdsReimbursementRequestStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.FinanceTdsReimbursementRequest -> m ())
updateStatusAndRejectionReason status rejectionReason id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.rejectionReason rejectionReason, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatusRejectionReasonAndAdminChecker ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.FinanceTdsReimbursementRequestStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.FinanceTdsReimbursementRequest -> m ())
updateStatusRejectionReasonAndAdminChecker status rejectionReason adminCheckerId adminCheckerName id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.rejectionReason rejectionReason,
      Se.Set Beam.adminCheckerId adminCheckerId,
      Se.Set Beam.adminCheckerName adminCheckerName,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatusRejectionReasonAndAdminMaker ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.FinanceTdsReimbursementRequestStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.FinanceTdsReimbursementRequest -> m ())
updateStatusRejectionReasonAndAdminMaker status rejectionReason adminMakerId adminMakerName id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.rejectionReason rejectionReason,
      Se.Set Beam.adminMakerId adminMakerId,
      Se.Set Beam.adminMakerName adminMakerName,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.FinanceTdsReimbursementRequest -> m (Maybe Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.FinanceTdsReimbursementRequest))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.FinanceTdsReimbursementRequest -> m ())
updateByPrimaryKey (Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.FinanceTdsReimbursementRequest {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.adminCheckerId adminCheckerId,
      Se.Set Beam.adminCheckerName adminCheckerName,
      Se.Set Beam.adminMakerId adminMakerId,
      Se.Set Beam.adminMakerName adminMakerName,
      Se.Set Beam.assessmentYear assessmentYear,
      Se.Set Beam.certAmount certAmount,
      Se.Set Beam.certNumber certNumber,
      Se.Set Beam.documentId (Kernel.Types.Id.getId documentId),
      Se.Set Beam.fleetOwnerId fleetOwnerId,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.quarter quarter,
      Se.Set Beam.rejectionReason rejectionReason,
      Se.Set Beam.status status,
      Se.Set Beam.tanNumber tanNumber,
      Se.Set Beam.tdsRate tdsRate,
      Se.Set Beam.tdsSection tdsSection,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
