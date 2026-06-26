{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RewardCampaignExtra
  ( updateEditableFields,
    updateStatus,
    findAllActiveInCityAtTime,
    findAllActive,
    findAllByCity,
  )
where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.RewardCampaign as DRCmp
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.RewardCampaign as Beam
import Storage.Queries.OrphanInstances.RewardCampaign ()

updateEditableFields ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DRCmp.RewardCampaign ->
  Maybe Text ->
  Maybe Text ->
  Maybe DRCmp.SponsorType ->
  Maybe Text ->
  Maybe Text ->
  Maybe DRCmp.CouponSourceType ->
  Maybe Text ->
  Maybe DRCmp.RedemptionTargetType ->
  Maybe Text ->
  Maybe DRCmp.ClaimMode ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Int ->
  m ()
updateEditableFields
  campaignId
  mName
  mDesc
  mSponsorType
  mSponsorName
  mSponsorLogoUrl
  mCouponSourceType
  mCouponTemplate
  mRedemptionTargetType
  mRedemptionTargetUrl
  mClaimMode
  mStartsAt
  mEndsAt
  mDisplayOrder = do
    now <- getCurrentTime
    let updates =
          catMaybes
            [ mName >>= \n -> Just (Se.Set Beam.name n),
              mDesc >>= \d -> Just (Se.Set Beam.description (Just d)),
              mSponsorType >>= \s -> Just (Se.Set Beam.sponsorType s),
              mSponsorName >>= \s -> Just (Se.Set Beam.sponsorName s),
              mSponsorLogoUrl >>= \u -> Just (Se.Set Beam.sponsorLogoUrl (Just u)),
              mCouponSourceType >>= \c -> Just (Se.Set Beam.couponSourceType c),
              mCouponTemplate >>= \t -> Just (Se.Set Beam.couponTemplate (Just t)),
              mRedemptionTargetType >>= \r -> Just (Se.Set Beam.redemptionTargetType r),
              mRedemptionTargetUrl >>= \u -> Just (Se.Set Beam.redemptionTargetUrl (Just u)),
              mClaimMode >>= \c -> Just (Se.Set Beam.claimMode c),
              mStartsAt >>= \t -> Just (Se.Set Beam.startsAt t),
              mEndsAt >>= \t -> Just (Se.Set Beam.endsAt (Just t)),
              mDisplayOrder >>= \o -> Just (Se.Set Beam.displayOrder o),
              Just (Se.Set Beam.updatedAt now)
            ]
    unless (null updates) $
      updateOneWithKV updates [Se.Is Beam.id (Se.Eq campaignId.getId)]

updateStatus ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DRCmp.RewardCampaign ->
  DRCmp.CampaignStatus ->
  m ()
updateStatus campaignId newStatus = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.status newStatus, Se.Set Beam.updatedAt now]
    [Se.Is Beam.id (Se.Eq campaignId.getId)]

findAllActiveInCityAtTime ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  m [DRCmp.RewardCampaign]
findAllActiveInCityAtTime moCityId atTime = do
  campaigns <-
    findAllWithKV
      [ Se.And
          [ Se.Is Beam.status (Se.Eq DRCmp.Active),
            Se.Is Beam.merchantOperatingCityId (Se.Eq moCityId.getId),
            Se.Is Beam.startsAt (Se.LessThanOrEq atTime)
          ]
      ]
  pure $ filter (\c -> maybe True (> atTime) c.endsAt) campaigns

findAllActive ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  m [DRCmp.RewardCampaign]
findAllActive =
  findAllWithKV [Se.Is Beam.status (Se.Eq DRCmp.Active)]

findAllByCity ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  m [DRCmp.RewardCampaign]
findAllByCity moCityId =
  findAllWithKV [Se.Is Beam.merchantOperatingCityId (Se.Eq moCityId.getId)]
