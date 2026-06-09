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
import qualified Crypto.Hash as Hash
import Data.Char (isDigit)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Domain.Action.UI.Registration as DRegistration
import qualified Environment
import Kernel.Prelude
import Kernel.Types.Id (ShortId (..))
import Kernel.Types.SlidingWindowLimiter (APIRateLimitOptions (..))
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import qualified PartnerAuth.Interface as PartnerAuth
import qualified PartnerAuth.Types as PartnerAuth
import qualified SharedLogic.MerchantConfig as SMC
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Error
import qualified Tools.PartnerAuth as TPartnerAuth

-- | The NammaYatri merchant under which partner sessions are issued.
nammaYatriShortId :: ShortId a
nammaYatriShortId = ShortId "NAMMA_YATRI"

-- | Per-(provider, token) throttle applied BEFORE any partner S2S call. This
-- caps token replay and the 1->2 S2S amplification a single token can drive.
-- Per-IP / global flood protection is the API gateway's responsibility (this
-- service sits behind it) and is intentionally not duplicated here.
partnerAuthRateLimitOptions :: APIRateLimitOptions
partnerAuthRateLimitOptions = APIRateLimitOptions {limit = 10, limitResetTimeInSec = 60}

-- | Non-reversible digest of the partner token, used only to namespace the
-- rate-limit key in Redis (never store the raw bearer token in a key).
tokenDigest :: Text -> Text
tokenDigest = T.pack . show . Hash.hashWith Hash.SHA256 . TE.encodeUtf8

postPartnerSession :: Text -> Maybe Text -> APT.PartnerSessionReq -> Environment.Flow APT.PartnerSessionRes
postPartnerSession providerTxt mbXForwardedFor req = do
  -- Wrap the entire partner interaction: invalid token, unknown/unconfigured
  -- provider, blocked IP, rate-limit breach, or any thrown (synchronous) error
  -- all map to 200 {isValid:false} + a log line, preserving the never-500
  -- contract. Control.Exception.Safe.try lets async exceptions (shutdown,
  -- timeouts) propagate instead of being swallowed.
  result <- CES.try @_ @SomeException issueSession
  case result of
    Right res -> pure res
    Left err -> do
      logError $ "PartnerAuth session failed for provider " <> providerTxt <> ": " <> show err
      pure invalidRes
  where
    invalidRes = APT.PartnerSessionRes {isValid = False, sessionToken = Nothing, mobileNumber = Nothing, name = Nothing}
    -- Bound the attacker-controlled path segment before it reaches a log line or
    -- a Redis key.
    providerKey = T.take 32 (T.toLower providerTxt)
    issueSession = do
      -- IP fraud gate first — mirror Registration.auth: reject a client IP that
      -- the merchant has flagged BEFORE any S2S call or Person resolve/create, so
      -- a blocked IP can neither create an account nor amplify into BHIM. The
      -- throw is caught by the wrapper above and surfaces as isValid:false (we
      -- deliberately don't reveal the block to the caller).
      whenJust (clientIpFromXForwardedFor mbXForwardedFor) $ \clientIP -> do
        ipBlocked <- SMC.isIPBlocked clientIP
        when ipBlocked $ throwError IpHitsLimitExceeded
      -- Then throttle per (provider, token), before parsing/lookup/S2S, so even
      -- unknown providers and repeated tokens are cheap to reject and cannot
      -- amplify into BHIM.
      checkSlidingWindowLimitWithOptions
        ("BAP:PartnerAuth:" <> providerKey <> ":token:" <> tokenDigest req.token)
        partnerAuthRateLimitOptions
      provider <-
        PartnerAuth.parsePartnerAuthService providerTxt
          & fromMaybeM (InvalidRequest $ "Unknown partner auth provider: " <> providerKey)
      merchant <-
        QMerchant.findByShortId nammaYatriShortId
          >>= fromMaybeM (MerchantNotFound "NAMMA_YATRI")
      merchantOpCity <-
        CQMOC.findByMerchantShortIdAndCity nammaYatriShortId merchant.defaultCity
          >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: NAMMA_YATRI ,city: " <> show merchant.defaultCity)
      cfg <-
        TPartnerAuth.getPartnerAuthConfig merchant.id merchantOpCity.id provider
          >>= fromMaybeM (InternalError $ "PartnerAuth config not found for provider: " <> providerKey)
      valid <- PartnerAuth.verifyToken cfg req.token
      if not valid
        then pure invalidRes
        else do
          userDetails <- PartnerAuth.getUserDetails cfg req.token
          -- BHIM is an India-only (UPI) app; normalise the number to
          -- ("+91", <local 10-digit>) so we match an existing Person rather than
          -- forking a duplicate account. A number we cannot validate as a plausible
          -- Indian mobile fails closed (isValid:false) instead of creating a
          -- Person under a malformed identifier.
          (countryCode, localMobile) <-
            normalizeIndianMobile userDetails.mobileNumber
              & fromMaybeM (InvalidRequest "PartnerAuth: partner returned an unparseable Indian mobile number")
          -- Don't overwrite an existing NY user's display name with a blank.
          let mbName = let n = T.strip userDetails.name in if T.null n then Nothing else Just n
          authRes <-
            DRegistration.resolveAndIssueDirectSession
              merchant
              countryCode
              localMobile
              mbName
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
              logError $ "PartnerAuth: no session issued (blocked user?) for provider " <> providerKey
              pure invalidRes

-- | Extract the client IP from an X-Forwarded-For header value: first hop, port
-- stripped. Mirrors the derivation in 'Domain.Action.UI.Registration.auth' so the
-- partner facade applies the same fraud gate.
clientIpFromXForwardedFor :: Maybe Text -> Maybe Text
clientIpFromXForwardedFor mbXForwardedFor =
  mbXForwardedFor >>= \headerValue ->
    listToMaybe (T.splitOn "," headerValue) >>= \firstIP ->
      let ipWithoutPort = T.takeWhile (/= ':') (T.strip firstIP)
       in if T.null ipWithoutPort then Nothing else Just ipWithoutPort

-- | Normalise an Indian mobile number from a partner to ("+91", <local 10-digit>),
-- tolerating bare, "91…", "+91…", "0091…" and spaced formats. Returns Nothing for
-- anything that is not a plausible Indian mobile (after stripping a country prefix,
-- exactly 10 digits beginning 6–9), so callers can fail closed instead of matching
-- or creating a Person under a malformed identifier. Kept separate so the
-- country-code/format assumption lives in one unit-tested place.
normalizeIndianMobile :: Text -> Maybe (Text, Text)
normalizeIndianMobile raw =
  let localDigits = T.takeEnd 10 (T.filter isDigit raw)
   in if T.length localDigits == 10 && maybe False ((`elem` ['6' .. '9']) . fst) (T.uncons localDigits)
        then Just ("+91", localDigits)
        else Nothing
