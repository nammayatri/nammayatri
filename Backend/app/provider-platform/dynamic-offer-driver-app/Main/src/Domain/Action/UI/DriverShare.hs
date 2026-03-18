module Domain.Action.UI.DriverShare
  ( postDriverShareMilestone,
    postDriverShareEarnings,
    postDriverShareReferral,
    postDriverShareBadge,
    postDriverShareTrack,
  )
where

import qualified API.Types.UI.DriverShare as API
import qualified Domain.Types.DriverReferral as DR
import qualified Domain.Types.DriverShareEvent as DSE
import qualified Domain.Types.DriverStats as DTS
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import qualified Domain.Types.ShareTemplate as ST
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.ShareCardGenerator as SCG
import qualified Storage.Queries.DriverReferral as QDR
import qualified Storage.Queries.DriverShareEvent as QDSE
import qualified Storage.Queries.DriverStats as QDS
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.ShareTemplate as QST
import Tools.Error

postDriverShareMilestone ::
  ( ( Id SP.Person,
      Id DM.Merchant,
      Id DMOC.MerchantOperatingCity
    ) ->
    API.ShareMilestoneReq ->
    Flow API.ShareCardRes
  )
postDriverShareMilestone (driverId, merchantId, merchantOpCityId) API.ShareMilestoneReq {..} = do
  person <- QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  driverStats <- runInReplica $ QDS.findByPrimaryKey driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  let driverName = fromMaybe "Driver" (person.firstName)
  templates <- QST.findByTemplateTypeAndMerchantId "MILESTONE" merchantId True
  let shareText = driverName <> " just hit " <> show milestoneValue <> " " <> milestoneType <> " on NammaYatri!"
  shareCardUrl <- SCG.generateMilestoneCard driverName milestoneType milestoneValue templates
  mbReferral <- runInReplica $ QDR.findById driverId
  let deepLinkUrl = SCG.generateDeepLink <$> ((.referralCode.getId) <$> mbReferral)
  pure $
    API.ShareCardRes
      { shareCardUrl = shareCardUrl,
        shareText = shareText,
        deepLinkUrl = deepLinkUrl
      }

postDriverShareEarnings ::
  ( ( Id SP.Person,
      Id DM.Merchant,
      Id DMOC.MerchantOperatingCity
    ) ->
    API.ShareEarningsReq ->
    Flow API.ShareCardRes
  )
postDriverShareEarnings (driverId, merchantId, merchantOpCityId) API.ShareEarningsReq {..} = do
  person <- QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  let driverName = fromMaybe "Driver" (person.firstName)
  templates <- QST.findByTemplateTypeAndMerchantId "EARNINGS" merchantId True
  let shareText = driverName <> " earned Rs " <> show earningsAmount <> " " <> earningsPeriod <> " on NammaYatri! Join and start earning."
  shareCardUrl <- SCG.generateEarningsCard driverName earningsPeriod earningsAmount templates
  mbReferral <- runInReplica $ QDR.findById driverId
  let deepLinkUrl = SCG.generateDeepLink <$> ((.referralCode.getId) <$> mbReferral)
  pure $
    API.ShareCardRes
      { shareCardUrl = shareCardUrl,
        shareText = shareText,
        deepLinkUrl = deepLinkUrl
      }

postDriverShareReferral ::
  ( ( Id SP.Person,
      Id DM.Merchant,
      Id DMOC.MerchantOperatingCity
    ) ->
    Flow API.ShareReferralRes
  )
postDriverShareReferral (driverId, merchantId, merchantOpCityId) = do
  person <- QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  referral <- runInReplica $ QDR.findById driverId >>= fromMaybeM (InvalidRequest "No referral code found for driver")
  let driverName = fromMaybe "Driver" (person.firstName)
      refCode = referral.referralCode.getId
  templates <- QST.findByTemplateTypeAndMerchantId "REFERRAL" merchantId True
  let shareText = "Join NammaYatri and earn up to Rs 50,000/month! Use my referral code " <> refCode <> " to get started."
      deepLink = SCG.generateDeepLink refCode
  shareCardUrl <- SCG.generateReferralCard driverName refCode templates
  pure $
    API.ShareReferralRes
      { shareCardUrl = shareCardUrl,
        shareText = shareText,
        deepLinkUrl = deepLink,
        referralCode = refCode
      }

postDriverShareBadge ::
  ( ( Id SP.Person,
      Id DM.Merchant,
      Id DMOC.MerchantOperatingCity
    ) ->
    API.ShareBadgeReq ->
    Flow API.ShareCardRes
  )
postDriverShareBadge (driverId, merchantId, merchantOpCityId) API.ShareBadgeReq {..} = do
  person <- QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  let driverName = fromMaybe "Driver" (person.firstName)
  templates <- QST.findByTemplateTypeAndMerchantId "BADGE" merchantId True
  let shareText = driverName <> " earned the " <> badgeName <> " badge on NammaYatri!"
  shareCardUrl <- SCG.generateBadgeCard driverName badgeId badgeName templates
  mbReferral <- runInReplica $ QDR.findById driverId
  let deepLinkUrl = SCG.generateDeepLink <$> ((.referralCode.getId) <$> mbReferral)
  pure $
    API.ShareCardRes
      { shareCardUrl = shareCardUrl,
        shareText = shareText,
        deepLinkUrl = deepLinkUrl
      }

postDriverShareTrack ::
  ( ( Id SP.Person,
      Id DM.Merchant,
      Id DMOC.MerchantOperatingCity
    ) ->
    API.ShareTrackReq ->
    Flow APISuccess
  )
postDriverShareTrack (driverId, merchantId, merchantOpCityId) API.ShareTrackReq {..} = do
  eventId <- generateGUID
  now <- getCurrentTime
  let event =
        DSE.DriverShareEvent
          { id = eventId,
            driverId = driverId,
            shareType = show shareType,
            sharePlatform = show <$> sharePlatform,
            contentId = contentId,
            shareCardUrl = Nothing,
            deepLinkUrl = Nothing,
            merchantOperatingCityId = merchantOpCityId,
            merchantId = merchantId,
            createdAt = now
          }
  QDSE.create event
  pure Success
