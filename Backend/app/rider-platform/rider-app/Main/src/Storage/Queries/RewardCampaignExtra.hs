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
  Maybe Int ->
  Maybe UTCTime ->
  m ()
updateEditableFields campaignId mDesc mOrder mEndsAt = do
  now <- getCurrentTime
  let updates =
        catMaybes
          [ mDesc >>= \d -> Just (Se.Set Beam.description (Just d)),
            mOrder >>= \o -> Just (Se.Set Beam.displayOrder o),
            mEndsAt >>= \t -> Just (Se.Set Beam.endsAt (Just t)),
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
