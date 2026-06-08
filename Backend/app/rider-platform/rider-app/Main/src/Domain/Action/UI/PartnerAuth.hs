-- | BHIM "PWA Journey" auth facade — generic over partner providers.
-- POST /v2/partner/{provider}/session : verify the partner token (S2S), fetch
-- the user, and issue a NammaYatri session (reusing the /auth/signature
-- person-resolution + token-issuance path) under the NAMMA_YATRI merchant.
-- Invalid token, unknown provider, or ANY error/exception -> 200 {isValid:false}
-- (logged); never a 500 to the PWA.
module Domain.Action.UI.PartnerAuth (postPartnerSession, normalizeIndianMobile) where

import qualified API.Types.UI.PartnerAuth as APT
import API.Types.UI.PartnerAuthExtra ()
import qualified Control.Exception.Safe as CES
import Data.Char (isDigit)
import qualified Data.Text as T
import qualified Domain.Action.UI.Registration as DRegistration
import qualified Environment
import Kernel.Prelude
import Kernel.Types.Id (ShortId (..))
import Kernel.Utils.Common
import qualified PartnerAuth.Interface as PartnerAuth
import qualified PartnerAuth.Types as PartnerAuth
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Error
import qualified Tools.PartnerAuth as TPartnerAuth

postPartnerSession :: Text -> APT.PartnerSessionReq -> Environment.Flow APT.PartnerSessionRes
postPartnerSession providerTxt req = do
  -- Wrap the entire partner interaction: invalid token, unknown/unconfigured
  -- provider, or any thrown (synchronous) error all map to 200 {isValid:false} +
  -- a log line. Control.Exception.Safe.try lets async exceptions (shutdown,
  -- timeouts) propagate instead of being swallowed.
  result <- CES.try @_ @SomeException issueSession
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
          -- BHIM is an India-only (UPI) app; normalise the number to ("+91", <local>)
          -- so we match an existing Person instead of forking a duplicate account when
          -- BHIM sends it with a 91 / +91 / 0091 prefix.
          let (countryCode, localMobile) = normalizeIndianMobile userDetails.mobileNumber
          authRes <-
            DRegistration.resolveAndIssueDirectSession
              merchant
              countryCode
              localMobile
              (Just userDetails.name)
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
          -- A verified BHIM token can still map to a blocked NY user, for whom no
          -- session token is issued. Never return isValid:true with a null token.
          case authRes.token of
            Just tok ->
              pure
                APT.PartnerSessionRes
                  { isValid = True,
                    sessionToken = Just tok,
                    mobileNumber = Just userDetails.mobileNumber,
                    name = Just userDetails.name
                  }
            Nothing -> do
              logError $ "PartnerAuth: no session issued (blocked user?) for provider " <> providerTxt
              pure invalidRes

-- | Normalise an Indian mobile number from BHIM to ("+91", <local 10-digit>),
-- tolerating bare, "91…", "+91…", "0091…" and spaced formats. Kept separate so
-- the country-code/format assumption lives in one unit-tested place.
normalizeIndianMobile :: Text -> (Text, Text)
normalizeIndianMobile raw = ("+91", T.takeEnd 10 (T.filter isDigit raw))
