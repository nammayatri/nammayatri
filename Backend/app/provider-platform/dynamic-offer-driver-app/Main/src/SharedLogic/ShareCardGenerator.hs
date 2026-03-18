module SharedLogic.ShareCardGenerator
  ( generateMilestoneCard,
    generateEarningsCard,
    generateReferralCard,
    generateBadgeCard,
    generateDeepLink,
  )
where

import qualified Data.Text as T
import qualified Domain.Types.ShareTemplate as ST
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common

-- | Base URL for deep links (configurable per merchant in production)
deepLinkBaseUrl :: Text
deepLinkBaseUrl = "https://nammayatri.in/r/"

-- | Base URL for share card CDN
shareCardBaseUrl :: Text
shareCardBaseUrl = "https://cdn.nammayatri.in/shares/"

-- | Generate a deep link URL with embedded referral code
generateDeepLink :: Text -> Text
generateDeepLink referralCode = deepLinkBaseUrl <> referralCode

-- | Generate a milestone share card image URL.
-- In production, this would render an SVG template with driver data,
-- convert to PNG, upload to S3, and return the CDN URL.
-- For now, returns a deterministic URL based on the card parameters.
generateMilestoneCard ::
  (MonadFlow m, Log m) =>
  Text ->
  Text ->
  Int ->
  [ST.ShareTemplate] ->
  m Text
generateMilestoneCard driverName milestoneType milestoneValue templates = do
  logInfo $ "Generating milestone card for " <> driverName <> ": " <> milestoneType <> " = " <> show milestoneValue
  cardId <- generateGUID
  let cardUrl = shareCardBaseUrl <> "milestone-" <> cardId <> ".png"
  -- TODO: Implement actual SVG template rendering + S3 upload
  -- 1. Select template from `templates` list (first active or default)
  -- 2. Render SVG with driverName, milestoneType, milestoneValue
  -- 3. Convert SVG to PNG via rendering service
  -- 4. Upload to S3 using AWS.S3 pattern from DriverProfileQuestions
  -- 5. Return CDN URL
  pure cardUrl

-- | Generate an earnings share card image URL.
generateEarningsCard ::
  (MonadFlow m, Log m) =>
  Text ->
  Text ->
  Int ->
  [ST.ShareTemplate] ->
  m Text
generateEarningsCard driverName earningsPeriod earningsAmount templates = do
  logInfo $ "Generating earnings card for " <> driverName <> ": Rs " <> show earningsAmount <> " (" <> earningsPeriod <> ")"
  cardId <- generateGUID
  let cardUrl = shareCardBaseUrl <> "earnings-" <> cardId <> ".png"
  -- TODO: Implement actual rendering + S3 upload (same pattern as milestone)
  pure cardUrl

-- | Generate a referral share card image URL with QR code.
generateReferralCard ::
  (MonadFlow m, Log m) =>
  Text ->
  Text ->
  [ST.ShareTemplate] ->
  m Text
generateReferralCard driverName referralCode templates = do
  logInfo $ "Generating referral card for " <> driverName <> " with code " <> referralCode
  cardId <- generateGUID
  let cardUrl = shareCardBaseUrl <> "referral-" <> cardId <> ".png"
  -- TODO: Implement actual rendering + S3 upload
  -- Include QR code encoding the deep link URL
  pure cardUrl

-- | Generate a badge share card image URL.
generateBadgeCard ::
  (MonadFlow m, Log m) =>
  Text ->
  Text ->
  Text ->
  [ST.ShareTemplate] ->
  m Text
generateBadgeCard driverName badgeId badgeName templates = do
  logInfo $ "Generating badge card for " <> driverName <> ": " <> badgeName <> " (" <> badgeId <> ")"
  cardId <- generateGUID
  let cardUrl = shareCardBaseUrl <> "badge-" <> cardId <> ".png"
  -- TODO: Implement actual rendering + S3 upload
  pure cardUrl
