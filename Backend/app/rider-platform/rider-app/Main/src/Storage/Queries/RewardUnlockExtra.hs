{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RewardUnlockExtra
  ( findByCampaignCohortAndCode,
    createIfNoActive,
    updateViewAndClaimTimestamps,
    markRedeemed,
    markActiveAsExpiredIfValidityPassed,
    findAllByCampaign,
  )
where

import Control.Exception (SomeException)
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RewardCampaign as DRCmp
import qualified Domain.Types.RewardCohort as DRC
import qualified Domain.Types.RewardUnlock as DRU
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.RewardUnlock as Beam
import Storage.Queries.OrphanInstances.RewardUnlock ()

findByCampaignCohortAndCode ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DRCmp.RewardCampaign ->
  Id DRC.RewardCohort ->
  Text ->
  m (Maybe DRU.RewardUnlock)
findByCampaignCohortAndCode campaignId cohortId code =
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.campaignId (Se.Eq campaignId.getId),
          Se.Is Beam.cohortId (Se.Eq cohortId.getId),
          Se.Is Beam.couponCode (Se.Eq (Just code)),
          Se.Is Beam.status (Se.Not (Se.Eq DRU.Reclaimed))
        ]
    ]

createIfNoActive ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  DRU.RewardUnlock ->
  m Bool
createIfNoActive unlock = do
  existing <-
    findOneWithKV
      [ Se.And
          [ Se.Is Beam.personId (Se.Eq unlock.personId.getId),
            Se.Is Beam.campaignId (Se.Eq unlock.campaignId.getId),
            Se.Is Beam.cohortId (Se.Eq unlock.cohortId.getId),
            Se.Is Beam.status (Se.Not (Se.Eq DRU.Reclaimed))
          ]
      ]
  case existing of
    Just _ -> pure False
    Nothing -> do
      result <- try @_ @SomeException $ createWithKV unlock
      case result of
        Right _ -> pure True
        Left _ -> pure False

updateViewAndClaimTimestamps ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DRU.RewardUnlock ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  m ()
updateViewAndClaimTimestamps unlockId mViewed mClaimed =
  when (isJust mViewed || isJust mClaimed) $ do
    now <- getCurrentTime
    updateOneWithKV
      ( catMaybes
          [ mViewed >>= \t -> Just (Se.Set Beam.viewedAt (Just t)),
            mClaimed >>= \t -> Just (Se.Set Beam.claimedAt (Just t)),
            Just (Se.Set Beam.updatedAt now)
          ]
      )
      [Se.Is Beam.id (Se.Eq unlockId.getId)]

markRedeemed ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DRU.RewardUnlock ->
  UTCTime ->
  m ()
markRedeemed unlockId now =
  updateOneWithKV
    [ Se.Set Beam.status DRU.Redeemed,
      Se.Set Beam.redeemedAt (Just now),
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.id (Se.Eq unlockId.getId)]

markActiveAsExpiredIfValidityPassed ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DP.Person ->
  UTCTime ->
  m ()
markActiveAsExpiredIfValidityPassed personId now =
  void $
    updateWithKV
      [ Se.Set Beam.status DRU.ExpiredUnredeemed,
        Se.Set Beam.updatedAt now
      ]
      [ Se.Is Beam.personId (Se.Eq personId.getId),
        Se.Is Beam.status (Se.Eq DRU.Active),
        Se.Is Beam.couponValidTill (Se.LessThan (Just now))
      ]

findAllByCampaign ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DRCmp.RewardCampaign ->
  m [DRU.RewardUnlock]
findAllByCampaign campaignId =
  findAllWithKV [Se.Is Beam.campaignId (Se.Eq campaignId.getId)]
