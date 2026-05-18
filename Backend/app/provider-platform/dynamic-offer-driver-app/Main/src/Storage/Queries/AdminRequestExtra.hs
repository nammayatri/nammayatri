module Storage.Queries.AdminRequestExtra where

import qualified Domain.Types.AdminRequest as DAdminRequest
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.AdminRequest as BeamAR
import Storage.Queries.OrphanInstances.AdminRequest ()

-- Extra code goes here --

findAllAdminRequestItems ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe (Id DAdminRequest.AdminRequest) ->
  Maybe DAdminRequest.AdminRequestStatus ->
  Maybe (Id DP.Person) ->
  Maybe DAdminRequest.ActionType ->
  Maybe DAdminRequest.AdjustmentType ->
  Maybe Text ->
  Maybe Text ->
  Maybe DAdminRequest.ReferenceTable ->
  Maybe DAdminRequest.AdjustmentSource ->
  Maybe (Id DP.Person) ->
  Maybe (Id DP.Person) ->
  Maybe (Id DP.Person) ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Int ->
  Int ->
  m [DAdminRequest.AdminRequest]
findAllAdminRequestItems merchantOperatingCityId mbAdminRequestId mbStatus mbPersonId mbActionType mbAdjustmentType mbReferenceType mbReferenceId mbReferenceTable mbSource mbAdminMakerId mbAdminCheckerId mbExcludedAdminMakerId mbFrom mbTo limit offset = do
  findAllWithOptionsKV
    [ Se.And $
        [Se.Is BeamAR.merchantOperatingCityId $ Se.Eq merchantOperatingCityId.getId]
          <> maybe [] (\adminRequestId -> [Se.Is BeamAR.id $ Se.Eq adminRequestId.getId]) mbAdminRequestId
          <> maybe [] (\status -> [Se.Is BeamAR.status $ Se.Eq status]) mbStatus
          <> maybe [] (\personId -> [Se.Is BeamAR.personId $ Se.Eq personId.getId]) mbPersonId
          <> maybe [] (\actionType -> [Se.Is BeamAR.actionType $ Se.Eq actionType]) mbActionType
          <> maybe [] (\adjustmentType -> [Se.Is BeamAR.adjustmentType $ Se.Eq (Just adjustmentType)]) mbAdjustmentType
          <> maybe [] (\referenceType -> [Se.Is BeamAR.referenceType $ Se.Eq (Just referenceType)]) mbReferenceType
          <> maybe [] (\referenceId -> [Se.Is BeamAR.referenceId $ Se.Eq referenceId]) mbReferenceId
          <> maybe [] (\referenceTable -> [Se.Is BeamAR.referenceTable $ Se.Eq referenceTable]) mbReferenceTable
          <> maybe [] (\source -> [Se.Is BeamAR.source $ Se.Eq (Just source)]) mbSource
          <> maybe [] (\adminMakerId -> [Se.Is BeamAR.adminMakerId $ Se.Eq adminMakerId.getId]) mbAdminMakerId
          <> maybe [] (\adminCheckerId -> [Se.Is BeamAR.adminCheckerId $ Se.Eq (Just adminCheckerId.getId)]) mbAdminCheckerId
          <> maybe [] (\excludedAdminMakerId -> [Se.Is BeamAR.adminMakerId $ Se.Not $ Se.Eq excludedAdminMakerId.getId]) mbExcludedAdminMakerId
          <> maybe [] (\from -> [Se.Is BeamAR.createdAt $ Se.GreaterThanOrEq from]) mbFrom
          <> maybe [] (\to -> [Se.Is BeamAR.createdAt $ Se.LessThanOrEq to]) mbTo
    ]
    (Se.Desc BeamAR.createdAt)
    (Just limit)
    (Just offset)
