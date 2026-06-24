{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RewardCohortExtra
  ( updateEditableFields,
  )
where

import qualified Data.Aeson as A
import qualified Domain.Types.RewardCohort as DRC
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.RewardCohort as Beam
import Storage.Queries.OrphanInstances.RewardCohort ()

updateEditableFields ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DRC.RewardCohort ->
  Maybe Text ->
  Maybe Text ->
  Maybe Int ->
  Maybe A.Value ->
  Maybe Text ->
  Maybe Text ->
  Maybe Int ->
  Maybe A.Value ->
  m ()
updateEditableFields cohortId mName mDesc mOrder mEligibility mRewardTitle mRewardImageUrl mCouponValidityDays mPresentation = do
  now <- getCurrentTime
  let updates =
        catMaybes
          [ mName >>= \n -> Just (Se.Set Beam.name n),
            mDesc >>= \d -> Just (Se.Set Beam.description (Just d)),
            mOrder >>= \o -> Just (Se.Set Beam.displayOrder o),
            mEligibility >>= \e -> Just (Se.Set Beam.eligibilityJsonLogic e),
            mRewardTitle >>= \t -> Just (Se.Set Beam.rewardTitle t),
            mRewardImageUrl >>= \u -> Just (Se.Set Beam.rewardImageUrl (Just u)),
            mCouponValidityDays >>= \d -> Just (Se.Set Beam.couponValidityDays (Just d)),
            mPresentation >>= \p -> Just (Se.Set Beam.presentation (Just p)),
            Just (Se.Set Beam.updatedAt now)
          ]
  unless (null updates) $
    updateOneWithKV updates [Se.Is Beam.id (Se.Eq cohortId.getId)]
