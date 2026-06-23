{-# LANGUAGE OverloadedStrings #-}

module Domain.Action.Dashboard.Rewards
  ( postRewardsCampaign,
    putRewardsCampaign,
    postRewardsCampaignCohort,
    postRewardsCampaignCohortCodes,
    postRewardsCampaignStatus,
    getRewardsCampaign,
    getRewardsCampaigns,
    getRewardsCampaignStats,
    postRewardsTriggerEval,
  )
where

import qualified API.Types.RiderPlatform.Management.Rewards as API
import qualified Dashboard.Common
import qualified Dashboard.Common.Rewards.CampaignStatus as CampaignStatus
import qualified Dashboard.Common.Rewards.ClaimMode as ClaimMode
import qualified Dashboard.Common.Rewards.CouponSourceType as CouponSourceType
import qualified Dashboard.Common.Rewards.RedemptionTargetType as RedemptionTargetType
import qualified Dashboard.Common.Rewards.SponsorType as SponsorType
import qualified Dashboard.RiderPlatform.Management.Rewards as Dash
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Csv (HasHeader (..), decode)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Domain.Action.Rewards.Producer as RewardsProducer
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.RewardCampaign as DRCmp
import qualified Domain.Types.RewardCohort as DRC
import qualified Domain.Types.RewardCouponUploadBatch as DRUB
import qualified Domain.Types.RewardUnlock as DRU
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RewardCampaign as QRCmp
import qualified Storage.Queries.RewardCampaignExtra as QRCmpE
import qualified Storage.Queries.RewardCohort as QRC
import qualified Storage.Queries.RewardCouponUploadBatch as QRUB
import qualified Storage.Queries.RewardUnlockExtra as QRUE
import qualified Tools.Rewards.RedisPool as Pool

castId :: Id a -> Id b
castId (Id x) = Id x

postRewardsCampaign ::
  ShortId DM.Merchant ->
  Context.City ->
  API.CreateCampaignReq ->
  Flow API.CreateCampaignResp
postRewardsCampaign merchantShortId opCity req = do
  (merchant, merchantOpCity) <- loadMerchantContext merchantShortId opCity
  campaignId <- generateGUID
  now <- getCurrentTime
  let campaign =
        DRCmp.RewardCampaign
          { id = campaignId,
            merchantId = merchant.id,
            merchantOperatingCityId = merchantOpCity.id,
            name = req.name,
            description = req.description,
            sponsorType = toDomainSponsorType req.sponsorType,
            sponsorName = req.sponsorName,
            sponsorLogoUrl = req.sponsorLogoUrl,
            couponSourceType = toDomainCouponSourceType req.couponSourceType,
            couponTemplate = req.couponTemplate,
            redemptionTargetType = toDomainRedemptionTargetType req.redemptionTargetType,
            redemptionTargetUrl = req.redemptionTargetUrl,
            claimMode = toDomainClaimMode req.claimMode,
            reclaimPolicy = Nothing,
            startsAt = req.startsAt,
            endsAt = req.endsAt,
            status = DRCmp.Draft,
            displayOrder = req.displayOrder,
            createdBy = "dashboard",
            createdAt = now,
            updatedAt = now
          }
  QRCmp.create campaign
  pure $ API.CreateCampaignResp (castId campaignId)

putRewardsCampaign ::
  ShortId DM.Merchant ->
  Context.City ->
  Id API.RewardCampaign ->
  API.EditCampaignReq ->
  Flow APISuccess
putRewardsCampaign merchantShortId opCity campaignId req = do
  c <- loadAuthorizedCampaign merchantShortId opCity (castId campaignId)
  case c.status of
    DRCmp.Draft -> pure ()
    _ ->
      case req.endsAt of
        Just newEndsAt
          | Just oldEndsAt <- c.endsAt,
            newEndsAt < oldEndsAt ->
            throwError (InvalidRequest "endsAt can only be extended, not shrunk")
        _ -> pure ()
  QRCmpE.updateEditableFields c.id req.description req.displayOrder req.endsAt
  pure Success

postRewardsCampaignCohort ::
  ShortId DM.Merchant ->
  Context.City ->
  Id API.RewardCampaign ->
  API.CreateCohortReq ->
  Flow API.CreateCohortResp
postRewardsCampaignCohort merchantShortId opCity campaignId req = do
  c <- loadAuthorizedCampaign merchantShortId opCity (castId campaignId)
  unless (c.status == DRCmp.Draft) $
    throwError (InvalidRequest "Cohorts can only be added to Draft campaigns")
  whenJust req.presentation validatePresentation
  cohortId <- generateGUID
  now <- getCurrentTime
  let cohort =
        DRC.RewardCohort
          { id = cohortId,
            campaignId = c.id,
            name = req.name,
            description = req.description,
            displayOrder = req.displayOrder,
            eligibilityJsonLogic = req.eligibilityJsonLogic,
            rewardTitle = req.rewardTitle,
            rewardImageUrl = req.rewardImageUrl,
            couponValidityDays = req.couponValidityDays,
            presentation = req.presentation,
            merchantId = Nothing,
            merchantOperatingCityId = Nothing,
            createdAt = now,
            updatedAt = now
          }
  QRC.create cohort
  pure $ API.CreateCohortResp (castId cohortId)

postRewardsCampaignCohortCodes ::
  ShortId DM.Merchant ->
  Context.City ->
  Id API.RewardCampaign ->
  Id API.RewardCohort ->
  Dash.UploadCodesReq ->
  Flow API.UploadCodesResp
postRewardsCampaignCohortCodes merchantShortId opCity campaignId cohortId req = do
  let domainCampaignId = castId campaignId
      domainCohortId = castId cohortId
  campaign <- loadAuthorizedCampaign merchantShortId opCity domainCampaignId
  unless (campaign.couponSourceType == DRCmp.Pool) $
    throwError (InvalidRequest "Coupon upload is only supported for Pool campaigns")
  when (campaign.status == DRCmp.Ended) $
    throwError (InvalidRequest "Cannot upload codes to an Ended campaign")
  cohort <- QRC.findById domainCohortId >>= fromMaybeM (InvalidRequest "Cohort not found")
  unless (cohort.campaignId == domainCampaignId) $
    throwError (InvalidRequest "Cohort does not belong to this campaign")
  rawBytes <- liftIO $ BS.readFile req.codesCsv
  codes <- case parseCodes rawBytes of
    [] -> throwError (InvalidRequest "CSV contains no codes")
    parsed -> pure parsed
  uploadBatchId <- generateGUIDText
  let s3Key = "rewards/" <> campaignId.getId <> "/" <> cohortId.getId <> "/" <> uploadBatchId <> ".csv"
  s3env <- asks (.s3Env)
  s3env.putRawH (T.unpack s3Key) rawBytes "text/csv"
  Pool.bulkSeedPool domainCampaignId domainCohortId codes
  now <- getCurrentTime
  batchId <- generateGUID
  QRUB.create $
    DRUB.RewardCouponUploadBatch
      { id = batchId,
        campaignId = domainCampaignId,
        cohortId = domainCohortId,
        uploadBatchId = uploadBatchId,
        s3Key = s3Key,
        totalCodesUploaded = length codes,
        uploadedBy = "dashboard",
        uploadedAt = now,
        createdAt = now,
        updatedAt = now,
        merchantId = Just campaign.merchantId,
        merchantOperatingCityId = Just campaign.merchantOperatingCityId
      }
  pure $ API.UploadCodesResp uploadBatchId

postRewardsCampaignStatus ::
  ShortId DM.Merchant ->
  Context.City ->
  Id API.RewardCampaign ->
  API.SetStatusReq ->
  Flow APISuccess
postRewardsCampaignStatus merchantShortId opCity campaignId req = do
  c <- loadAuthorizedCampaign merchantShortId opCity (castId campaignId)
  let newStatus = toDomainCampaignStatus req.newStatus
  validateTransition c.status newStatus
  when (newStatus == DRCmp.Active) $ do
    cohorts <- QRC.findAllByCampaign c.id
    when (null cohorts) $ throwError (InvalidRequest "Activate requires at least one cohort")
    forM_ cohorts $ \co ->
      when (c.couponSourceType == DRCmp.Pool) $ do
        size <- Pool.poolSize c.id co.id
        when (size == 0) $
          throwError (InvalidRequest $ "Cohort " <> co.name <> " has empty pool")
  QRCmpE.updateStatus c.id newStatus
  pure Success

getRewardsCampaign ::
  ShortId DM.Merchant ->
  Context.City ->
  Id API.RewardCampaign ->
  Flow API.CampaignDetails
getRewardsCampaign merchantShortId opCity campaignId = do
  c <- loadAuthorizedCampaign merchantShortId opCity (castId campaignId)
  cohorts <- QRC.findAllByCampaign c.id
  pure $ mkCampaignDetails c cohorts

getRewardsCampaigns ::
  ShortId DM.Merchant ->
  Context.City ->
  Flow [API.CampaignDetails]
getRewardsCampaigns merchantShortId opCity = do
  (_merchant, merchantOpCity) <- loadMerchantContext merchantShortId opCity
  cs <- QRCmpE.findAllByCity merchantOpCity.id
  forM cs $ \c -> do
    cohorts <- QRC.findAllByCampaign c.id
    pure $ mkCampaignDetails c cohorts

getRewardsCampaignStats ::
  ShortId DM.Merchant ->
  Context.City ->
  Id API.RewardCampaign ->
  Flow API.CampaignStats
getRewardsCampaignStats merchantShortId opCity campaignId = do
  campaign <- loadAuthorizedCampaign merchantShortId opCity (castId campaignId)
  cohorts <- QRC.findAllByCampaign campaign.id
  unlocks <- QRUE.findAllByCampaign campaign.id
  let totalUnlocks = length unlocks
      unlocksByCohort =
        [ API.CohortCount (castId co.id) (length (filter (\u -> u.cohortId == co.id) unlocks))
          | co <- cohorts
        ]
      expiredUnredeemedCount = length (filter (\u -> u.status == DRU.ExpiredUnredeemed) unlocks)
      claimedCount = length (filter (isJust . (.claimedAt)) unlocks)
      claimRate =
        if totalUnlocks == 0
          then 0
          else fromIntegral claimedCount / fromIntegral totalUnlocks
  poolRemaining <-
    forM cohorts $ \co -> do
      size <- Pool.poolSize campaign.id co.id
      pure $ API.CohortPoolRemaining (castId co.id) size
  pure
    API.CampaignStats
      { totalUnlocks = totalUnlocks,
        unlocksByCohort = unlocksByCohort,
        poolRemainingByCohort = poolRemaining,
        claimRate = claimRate,
        expiredUnredeemedCount = expiredUnredeemedCount
      }

postRewardsTriggerEval ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Dashboard.Common.Person ->
  Flow APISuccess
postRewardsTriggerEval merchantShortId opCity personId = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  person <- QPerson.findById (castId personId) >>= fromMaybeM (PersonDoesNotExist personId.getId)
  unless (person.merchantId == merchant.id && person.merchantOperatingCityId == merchantOpCity.id) $
    throwError (PersonDoesNotExist personId.getId)
  now <- getCurrentTime
  RewardsProducer.publishRewardEvalRequested (castId personId) merchantOpCity.id now
  pure Success

loadMerchantContext ::
  ShortId DM.Merchant ->
  Context.City ->
  Flow (DM.Merchant, DMOC.MerchantOperatingCity)
loadMerchantContext merchantShortId opCity = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  pure (merchant, merchantOpCity)

loadAuthorizedCampaign ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DRCmp.RewardCampaign ->
  Flow DRCmp.RewardCampaign
loadAuthorizedCampaign merchantShortId opCity campaignId = do
  (merchant, merchantOpCity) <- loadMerchantContext merchantShortId opCity
  campaign <- QRCmp.findById campaignId >>= fromMaybeM (InvalidRequest "Campaign not found")
  unless (campaign.merchantId == merchant.id && campaign.merchantOperatingCityId == merchantOpCity.id) $
    throwError (InvalidRequest "Campaign does not belong to this merchant operating city")
  pure campaign

validateTransition :: DRCmp.CampaignStatus -> DRCmp.CampaignStatus -> Flow ()
validateTransition DRCmp.Draft DRCmp.Active = pure ()
validateTransition DRCmp.Active DRCmp.Paused = pure ()
validateTransition DRCmp.Paused DRCmp.Active = pure ()
validateTransition DRCmp.Active DRCmp.Ended = pure ()
validateTransition DRCmp.Paused DRCmp.Ended = pure ()
validateTransition from to = throwError (InvalidRequest $ "Invalid transition: " <> show from <> " -> " <> show to)

mkCampaignDetails :: DRCmp.RewardCampaign -> [DRC.RewardCohort] -> API.CampaignDetails
mkCampaignDetails campaign cohorts =
  API.CampaignDetails
    { campaign = toApiCampaign campaign,
      cohorts = map toApiCohort cohorts
    }

toApiCampaign :: DRCmp.RewardCampaign -> API.RewardCampaign
toApiCampaign c =
  API.RewardCampaign
    { id = castId c.id,
      merchantId = castId c.merchantId,
      merchantOperatingCityId = castId c.merchantOperatingCityId,
      name = c.name,
      description = c.description,
      sponsorType = fromDomainSponsorType c.sponsorType,
      sponsorName = c.sponsorName,
      sponsorLogoUrl = c.sponsorLogoUrl,
      couponSourceType = fromDomainCouponSourceType c.couponSourceType,
      couponTemplate = c.couponTemplate,
      redemptionTargetType = fromDomainRedemptionTargetType c.redemptionTargetType,
      redemptionTargetUrl = c.redemptionTargetUrl,
      claimMode = fromDomainClaimMode c.claimMode,
      reclaimPolicy = c.reclaimPolicy,
      startsAt = c.startsAt,
      endsAt = c.endsAt,
      status = fromDomainCampaignStatus c.status,
      displayOrder = c.displayOrder,
      createdBy = c.createdBy,
      createdAt = c.createdAt,
      updatedAt = c.updatedAt
    }

toApiCohort :: DRC.RewardCohort -> API.RewardCohort
toApiCohort co =
  API.RewardCohort
    { id = castId co.id,
      campaignId = castId co.campaignId,
      name = co.name,
      description = co.description,
      displayOrder = co.displayOrder,
      eligibilityJsonLogic = co.eligibilityJsonLogic,
      rewardTitle = co.rewardTitle,
      rewardImageUrl = co.rewardImageUrl,
      couponValidityDays = co.couponValidityDays,
      presentation = co.presentation,
      createdAt = co.createdAt,
      updatedAt = co.updatedAt
    }

validatePresentation :: (MonadFlow m) => A.Value -> m ()
validatePresentation v = case v of
  A.Object o -> do
    case KM.lookup "schemaVersion" o of
      Just (A.Number n) | n == 1 -> pure ()
      Just _ -> throwError (InvalidRequest "presentation.schemaVersion must be 1")
      Nothing -> pure ()
    case KM.lookup "detail" o of
      Just (A.Object d) -> case KM.lookup "body" d of
        Just (A.Object b) -> case (KM.lookup "type" b, KM.lookup "content" b) of
          (Just (A.String "paragraph"), Just (A.String _)) -> pure ()
          (Just (A.String "list"), Just (A.Array _)) -> pure ()
          (Just (A.String "paragraph"), _) ->
            throwError (InvalidRequest "presentation.detail.body.content must be string when type=paragraph")
          (Just (A.String "list"), _) ->
            throwError (InvalidRequest "presentation.detail.body.content must be array when type=list")
          (Just _, _) -> throwError (InvalidRequest "presentation.detail.body.type must be 'paragraph' or 'list'")
          _ -> pure ()
        _ -> pure ()
      _ -> pure ()
  _ -> throwError (InvalidRequest "presentation must be a JSON object")

toDomainSponsorType :: SponsorType.SponsorType -> DRCmp.SponsorType
toDomainSponsorType SponsorType.Internal = DRCmp.Internal
toDomainSponsorType SponsorType.External = DRCmp.External

fromDomainSponsorType :: DRCmp.SponsorType -> SponsorType.SponsorType
fromDomainSponsorType DRCmp.Internal = SponsorType.Internal
fromDomainSponsorType DRCmp.External = SponsorType.External

toDomainCouponSourceType :: CouponSourceType.CouponSourceType -> DRCmp.CouponSourceType
toDomainCouponSourceType CouponSourceType.Pool = DRCmp.Pool
toDomainCouponSourceType CouponSourceType.Templated = DRCmp.Templated

fromDomainCouponSourceType :: DRCmp.CouponSourceType -> CouponSourceType.CouponSourceType
fromDomainCouponSourceType DRCmp.Pool = CouponSourceType.Pool
fromDomainCouponSourceType DRCmp.Templated = CouponSourceType.Templated

toDomainRedemptionTargetType :: RedemptionTargetType.RedemptionTargetType -> DRCmp.RedemptionTargetType
toDomainRedemptionTargetType RedemptionTargetType.Internal = DRCmp.InApp
toDomainRedemptionTargetType RedemptionTargetType.ExternalUrl = DRCmp.ExternalUrl
toDomainRedemptionTargetType RedemptionTargetType.ExternalManual = DRCmp.ExternalManual

fromDomainRedemptionTargetType :: DRCmp.RedemptionTargetType -> RedemptionTargetType.RedemptionTargetType
fromDomainRedemptionTargetType DRCmp.InApp = RedemptionTargetType.Internal
fromDomainRedemptionTargetType DRCmp.ExternalUrl = RedemptionTargetType.ExternalUrl
fromDomainRedemptionTargetType DRCmp.ExternalManual = RedemptionTargetType.ExternalManual

toDomainClaimMode :: ClaimMode.ClaimMode -> DRCmp.ClaimMode
toDomainClaimMode ClaimMode.AutoClaim = DRCmp.AutoClaim
toDomainClaimMode ClaimMode.ManualClaim = DRCmp.ManualClaim

fromDomainClaimMode :: DRCmp.ClaimMode -> ClaimMode.ClaimMode
fromDomainClaimMode DRCmp.AutoClaim = ClaimMode.AutoClaim
fromDomainClaimMode DRCmp.ManualClaim = ClaimMode.ManualClaim

toDomainCampaignStatus :: CampaignStatus.CampaignStatus -> DRCmp.CampaignStatus
toDomainCampaignStatus CampaignStatus.Draft = DRCmp.Draft
toDomainCampaignStatus CampaignStatus.Active = DRCmp.Active
toDomainCampaignStatus CampaignStatus.Paused = DRCmp.Paused
toDomainCampaignStatus CampaignStatus.Ended = DRCmp.Ended

fromDomainCampaignStatus :: DRCmp.CampaignStatus -> CampaignStatus.CampaignStatus
fromDomainCampaignStatus DRCmp.Draft = CampaignStatus.Draft
fromDomainCampaignStatus DRCmp.Active = CampaignStatus.Active
fromDomainCampaignStatus DRCmp.Paused = CampaignStatus.Paused
fromDomainCampaignStatus DRCmp.Ended = CampaignStatus.Ended

parseCodes :: BS.ByteString -> [Text]
parseCodes bs =
  case decode NoHeader (LBS.fromStrict bs) of
    Left _ -> []
    Right rows ->
      let firstColumn =
            [ T.strip (TE.decodeUtf8 cell)
              | row <- V.toList rows,
                cell : _ <- [V.toList row],
                not (BS.null cell)
            ]
       in case firstColumn of
            (h : rest) | T.toLower h == "code" -> filter (not . T.null) rest
            other -> filter (not . T.null) other
