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
  -- | 'Nothing' = leave alone; 'Just Nothing' = clear back to one-shot;
  -- 'Just (Just n)' = set the repeat cap to n. Double-'Maybe' (unlike the
  -- other fields here) because ops needs a way to unset a previously-set cap.
  Maybe (Maybe Int) ->
  m ()
updateEditableFields cohortId mName mDesc mOrder mEligibility mRewardTitle mRewardImageUrl mCouponValidityDays mPresentation mMaxUnlocksPerCohort = do
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
            mMaxUnlocksPerCohort >>= \mn -> Just (Se.Set Beam.maxUnlocksPerCohort mn),
            Just (Se.Set Beam.updatedAt now)
          ]
  unless (null updates) $
    updateOneWithKV updates [Se.Is Beam.id (Se.Eq cohortId.getId)]
