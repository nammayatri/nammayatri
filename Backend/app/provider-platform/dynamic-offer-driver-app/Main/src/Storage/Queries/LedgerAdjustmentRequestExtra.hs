{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.LedgerAdjustmentRequestExtra where

import qualified Domain.Types.LedgerAdjustmentRequest as DLA
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.LedgerAdjustmentRequest as BeamLAR
import Storage.Queries.OrphanInstances.LedgerAdjustmentRequest ()

-- Extra code goes here --

findAllLedgerAdjustmentItems ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe (Id DLA.LedgerAdjustmentRequest) ->
  Maybe DLA.AdjustmentRequestStatus ->
  Maybe (Id DP.Person) ->
  Maybe DLA.AdjustmentCategory ->
  Maybe DLA.AdjustmentDirection ->
  Maybe Text ->
  Maybe Text ->
  Maybe (Id DP.Person) ->
  Maybe (Id DP.Person) ->
  Maybe (Id DP.Person) ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Int ->
  Int ->
  m [DLA.LedgerAdjustmentRequest]
findAllLedgerAdjustmentItems merchantOperatingCityId mbAdjustmentRequestId mbStatus mbPersonId mbCategory mbDirection mbReferenceType mbReferenceId mbAdminMakerId mbAdminCheckerId mbExcludedAdminMakerId mbFrom mbTo limit offset = do
  findAllWithOptionsKV
    [ Se.And $
        [Se.Is BeamLAR.merchantOperatingCityId $ Se.Eq merchantOperatingCityId.getId]
          <> maybe [] (\adjustmentRequestId -> [Se.Is BeamLAR.id $ Se.Eq adjustmentRequestId.getId]) mbAdjustmentRequestId
          <> maybe [] (\status -> [Se.Is BeamLAR.status $ Se.Eq status]) mbStatus
          <> maybe [] (\personId -> [Se.Is BeamLAR.personId $ Se.Eq personId.getId]) mbPersonId
          <> maybe [] (\category -> [Se.Is BeamLAR.category $ Se.Eq category]) mbCategory
          <> maybe [] (\direction -> [Se.Is BeamLAR.direction $ Se.Eq direction]) mbDirection
          <> maybe [] (\referenceType -> [Se.Is BeamLAR.referenceType $ Se.Eq (Just referenceType)]) mbReferenceType
          <> maybe [] (\referenceId -> [Se.Is BeamLAR.referenceId $ Se.Eq (Just referenceId)]) mbReferenceId
          <> maybe [] (\adminMakerId -> [Se.Is BeamLAR.adminMakerId $ Se.Eq adminMakerId.getId]) mbAdminMakerId
          <> maybe [] (\adminCheckerId -> [Se.Is BeamLAR.adminCheckerId $ Se.Eq (Just adminCheckerId.getId)]) mbAdminCheckerId
          <> maybe [] (\excludedAdminMakerId -> [Se.Is BeamLAR.adminMakerId $ Se.Not $ Se.Eq excludedAdminMakerId.getId]) mbExcludedAdminMakerId
          <> maybe [] (\from -> [Se.Is BeamLAR.createdAt $ Se.GreaterThanOrEq from]) mbFrom
          <> maybe [] (\to -> [Se.Is BeamLAR.createdAt $ Se.LessThanOrEq to]) mbTo
    ]
    (Se.Desc BeamLAR.createdAt)
    (Just limit)
    (Just offset)
