{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Permit.Storage.Queries.PermitApplication (module Lib.Permit.Storage.Queries.PermitApplication, module ReExport) where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Permit.Domain.Types.PermitApplicant
import qualified Lib.Permit.Domain.Types.PermitApplication
import qualified Lib.Permit.Storage.Beam.BeamFlow
import qualified Lib.Permit.Storage.Beam.PermitApplication as Beam
import Lib.Permit.Storage.Queries.PermitApplicationExtra as ReExport
import qualified Sequelize as Se

create :: (Lib.Permit.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Permit.Domain.Types.PermitApplication.PermitApplication -> m ())
create = createWithKV

createMany :: (Lib.Permit.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Permit.Domain.Types.PermitApplication.PermitApplication] -> m ())
createMany = traverse_ create

findAllByApplicantId ::
  (Lib.Permit.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplicant.PermitApplicant -> m ([Lib.Permit.Domain.Types.PermitApplication.PermitApplication]))
findAllByApplicantId limit offset applicantId = do findAllWithOptionsKV [Se.Is Beam.applicantId $ Se.Eq (Kernel.Types.Id.getId applicantId)] (Se.Desc Beam.createdAt) limit offset

findById ::
  (Lib.Permit.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication -> m (Maybe Lib.Permit.Domain.Types.PermitApplication.PermitApplication))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByReferenceNo :: (Lib.Permit.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m (Maybe Lib.Permit.Domain.Types.PermitApplication.PermitApplication))
findByReferenceNo referenceNo = do findOneWithKV [Se.Is Beam.referenceNo $ Se.Eq referenceNo]

findByPrimaryKey ::
  (Lib.Permit.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication -> m (Maybe Lib.Permit.Domain.Types.PermitApplication.PermitApplication))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Permit.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Permit.Domain.Types.PermitApplication.PermitApplication -> m ())
updateByPrimaryKey (Lib.Permit.Domain.Types.PermitApplication.PermitApplication {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.applicantId (Kernel.Types.Id.getId applicantId),
      Se.Set Beam.approvalLevel approvalLevel,
      Se.Set Beam.consentAt consentAt,
      Se.Set Beam.decidedAt decidedAt,
      Se.Set Beam.decidedById decidedById,
      Se.Set Beam.decidedByName decidedByName,
      Se.Set Beam.decisionReason decisionReason,
      Se.Set Beam.detailsEncrypted (((details & unEncrypted . encrypted))),
      Se.Set Beam.detailsHash ((details & hash)),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.permitDate permitDate,
      Se.Set Beam.permitType permitType,
      Se.Set Beam.primaryConflictHash primaryConflictHash,
      Se.Set Beam.qrTokenEncrypted (((qrToken <&> unEncrypted . (.encrypted)))),
      Se.Set Beam.qrTokenHash ((qrToken <&> (.hash))),
      Se.Set Beam.referenceNo referenceNo,
      Se.Set Beam.secondaryConflictHash secondaryConflictHash,
      Se.Set Beam.status status,
      Se.Set Beam.submittedAt submittedAt,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.validFrom validFrom,
      Se.Set Beam.validTill validTill
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
