{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Permit.Storage.Queries.PermitApplicant (module Lib.Permit.Storage.Queries.PermitApplicant, module ReExport) where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Permit.Domain.Types.PermitApplicant
import qualified Lib.Permit.Storage.Beam.BeamFlow
import qualified Lib.Permit.Storage.Beam.PermitApplicant as Beam
import Lib.Permit.Storage.Queries.PermitApplicantExtra as ReExport
import qualified Sequelize as Se

create :: (Lib.Permit.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Permit.Domain.Types.PermitApplicant.PermitApplicant -> m ())
create = createWithKV

createMany :: (Lib.Permit.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Permit.Domain.Types.PermitApplicant.PermitApplicant] -> m ())
createMany = traverse_ create

findById ::
  (Lib.Permit.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplicant.PermitApplicant -> m (Maybe Lib.Permit.Domain.Types.PermitApplicant.PermitApplicant))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (Lib.Permit.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplicant.PermitApplicant -> m (Maybe Lib.Permit.Domain.Types.PermitApplicant.PermitApplicant))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Permit.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Permit.Domain.Types.PermitApplicant.PermitApplicant -> m ())
updateByPrimaryKey (Lib.Permit.Domain.Types.PermitApplicant.PermitApplicant {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.mobileCountryCode mobileCountryCode,
      Se.Set Beam.mobileNumberEncrypted (((mobileNumber & unEncrypted . encrypted))),
      Se.Set Beam.mobileNumberHash ((mobileNumber & hash)),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
