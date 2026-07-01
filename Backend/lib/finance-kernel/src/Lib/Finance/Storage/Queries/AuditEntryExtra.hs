module Lib.Finance.Storage.Queries.AuditEntryExtra where

import Kernel.Beam.Functions
import Kernel.Prelude
import Lib.Finance.Core.Types (ActorType)
import qualified Lib.Finance.Domain.Types.AuditEntry as Domain
import qualified Lib.Finance.Storage.Beam.AuditEntry as Beam
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import Lib.Finance.Storage.Queries.OrphanInstances.AuditEntry ()
import qualified Sequelize as Se

findByMerchantOpCityIdWithFilters ::
  (BeamFlow m r) =>
  Text -> -- merchantId
  Text -> -- merchantOperatingCityId
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Domain.AuditEntityType ->
  Maybe Domain.AuditAction ->
  Maybe ActorType -> -- actorType
  Maybe Text -> -- actorId
  Maybe Text -> -- entityId
  Maybe Int ->
  Maybe Int ->
  m [Domain.AuditEntry]
findByMerchantOpCityIdWithFilters merchantId merchantOpCityId mbFrom mbTo mbEntityType mbAction mbActorType mbActorId mbEntityId mbLimit mbOffset =
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOpCityId
        ]
          <> [Se.Is Beam.createdAt $ Se.GreaterThanOrEq from | Just from <- [mbFrom]]
          <> [Se.Is Beam.createdAt $ Se.LessThanOrEq to | Just to <- [mbTo]]
          <> [Se.Is Beam.entityType $ Se.Eq entityType | Just entityType <- [mbEntityType]]
          <> [Se.Is Beam.action $ Se.Eq action | Just action <- [mbAction]]
          <> [Se.Is Beam.actorType $ Se.Eq actorType | Just actorType <- [mbActorType]]
          <> [Se.Is Beam.actorId $ Se.Eq (Just actorId) | Just actorId <- [mbActorId]]
          <> [Se.Is Beam.entityId $ Se.Eq entityId | Just entityId <- [mbEntityId]]
    ]
    (Se.Desc Beam.createdAt)
    mbLimit
    mbOffset
