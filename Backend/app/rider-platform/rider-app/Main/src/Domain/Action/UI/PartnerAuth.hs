-- | BHIM "PWA Journey" auth facade — generic over partner providers.
-- POST /v2/partner/{provider}/session : verify the partner token (S2S), fetch
-- the user, and issue a NammaYatri session (reusing the /auth/signature
-- person-resolution + token-issuance path) under the NAMMA_YATRI merchant.
-- Invalid token, unknown provider, or ANY error/exception -> 200 {isValid:false}
-- (logged); never a 500 to the PWA.
module Domain.Action.UI.PartnerAuth (postPartnerSession) where

import qualified API.Types.UI.PartnerAuth as APT
import API.Types.UI.PartnerAuthExtra ()
import qualified Domain.Action.UI.Registration as DRegistration
import qualified Environment
import Kernel.Prelude
import Kernel.Types.Id (ShortId (..))
import Kernel.Utils.Common
import qualified PartnerAuth.Interface as PartnerAuth
import qualified PartnerAuth.Types as PartnerAuth
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Tools.PartnerAuth as TPartnerAuth
import Tools.Error

postPartnerSession :: Text -> APT.PartnerSessionReq -> Environment.Flow APT.PartnerSessionRes
postPartnerSession providerTxt req = do
  -- Wrap the entire partner interaction: invalid token, unknown/unconfigured
  -- provider, or any thrown error all map to 200 {isValid:false} + a log line.
  result <- try @_ @SomeException issueSession
  case result of
    Right res -> pure res
    Left err -> do
      logError $ "PartnerAuth session failed for provider " <> providerTxt <> ": " <> show err
      pure invalidRes
  where
    invalidRes = APT.PartnerSessionRes {isValid = False, sessionToken = Nothing, mobileNumber = Nothing, name = Nothing}
    issueSession = do
      provider <-
        PartnerAuth.parsePartnerAuthService providerTxt
          & fromMaybeM (InvalidRequest $ "Unknown partner auth provider: " <> providerTxt)
      merchant <-
        QMerchant.findByShortId (ShortId "NAMMA_YATRI")
          >>= fromMaybeM (MerchantNotFound "NAMMA_YATRI")
      merchantOpCity <-
        CQMOC.findByMerchantShortIdAndCity (ShortId "NAMMA_YATRI") merchant.defaultCity
          >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: NAMMA_YATRI ,city: " <> show merchant.defaultCity)
      cfg <-
        TPartnerAuth.getPartnerAuthConfig merchant.id merchantOpCity.id provider
          >>= fromMaybeM (InternalError $ "PartnerAuth config not found for provider: " <> providerTxt)
      valid <- PartnerAuth.verifyToken cfg req.token
      if not valid
        then pure invalidRes
        else do
          userDetails <- PartnerAuth.getUserDetails cfg req.token
          authRes <-
            -- BHIM is an India-only (UPI) app, so the country code is fixed to "+91".
            -- Derive/configure this when a non-India provider is onboarded.
            DRegistration.resolveAndIssueDirectSession
              merchant
              "+91"
              userDetails.mobileNumber
              (Just userDetails.name)
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
          pure
            APT.PartnerSessionRes
              { isValid = True,
                sessionToken = authRes.token,
                mobileNumber = Just userDetails.mobileNumber,
                name = Just userDetails.name
              }
