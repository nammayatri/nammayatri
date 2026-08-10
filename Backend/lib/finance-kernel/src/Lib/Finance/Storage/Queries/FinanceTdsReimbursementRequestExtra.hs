module Lib.Finance.Storage.Queries.FinanceTdsReimbursementRequestExtra where

import Kernel.Beam.Functions
import Kernel.Prelude
import Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Beam.FinanceTdsReimbursementRequest as Beam
import Lib.Finance.Storage.Queries.OrphanInstances.FinanceTdsReimbursementRequest ()
import qualified Sequelize as Se

findAllByMerchantOpCityIdWithFilters ::
  (BeamFlow m r) =>
  Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Quarter ->
  Maybe AssessmentYear ->
  Maybe FinanceTdsReimbursementRequestStatus ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Int ->
  Maybe Int ->
  m [FinanceTdsReimbursementRequest]
findAllByMerchantOpCityIdWithFilters merchantOpCityId mbFleetOwnerId mbTan mbQuarter mbAssessmentYear mbStatus mbFrom mbTo mbLimit mbOffset = do
  let limit = max 0 . min maxLimit . fromMaybe defaultLimit $ mbLimit
      offset = max 0 . fromMaybe 0 $ mbOffset
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOpCityId
        ]
          <> [Se.Is Beam.fleetOwnerId $ Se.Eq fid | Just fid <- [mbFleetOwnerId]]
          <> [Se.Is Beam.tanNumber $ Se.Eq t | Just t <- [mbTan]]
          <> [Se.Is Beam.quarter $ Se.Eq q | Just q <- [mbQuarter]]
          <> [Se.Is Beam.assessmentYear $ Se.Eq ay | Just ay <- [mbAssessmentYear]]
          <> [Se.Is Beam.status $ Se.Eq st | Just st <- [mbStatus]]
          <> [Se.Is Beam.createdAt $ Se.GreaterThanOrEq f | Just f <- [mbFrom]]
          <> [Se.Is Beam.createdAt $ Se.LessThanOrEq t | Just t <- [mbTo]]
    ]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)
  where
    maxLimit = 20
    defaultLimit = 10
