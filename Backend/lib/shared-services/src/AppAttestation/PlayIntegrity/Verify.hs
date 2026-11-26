{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module AppAttestation.PlayIntegrity.Verify
  ( VerdictChecks (..),
    verifyVerdict,
    normalizeCertDigest,
    activityLevel,
  )
where

import AppAttestation.Canonical (encodeBase64Url, stripBase64Padding)
import AppAttestation.Domain.Types.Common
import AppAttestation.PlayIntegrity.Types
import AppAttestation.Types (Reason (..))
import qualified Data.ByteString as BS
import Data.Char (digitToInt, isHexDigit)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Kernel.Prelude

data VerdictChecks = VerdictChecks
  { allowedApps :: [AndroidAppConfig],
    expectedRequestHash :: Text,
    now :: UTCTime,
    tokenMaxAgeSeconds :: Int,
    minDeviceVerdict :: AndroidDeviceVerdict,
    requirePlayRecognizedApp :: Bool,
    requireLicensedApp :: Bool,
    maxRecentDeviceActivityLevel :: Maybe Int,
    rejectedAppAccessRisks :: [Text]
  }
  deriving (Show)

verifyVerdict :: VerdictChecks -> TokenPayload -> [Reason]
verifyVerdict VerdictChecks {..} payload =
  catMaybes
    [ check VERDICT_MALFORMED (isJust payload.requestDetails && isJust payload.appIntegrity && isJust payload.deviceIntegrity),
      check PACKAGE_MISMATCH packageMatches,
      check REQUEST_HASH_MISMATCH requestHashMatches,
      check TOKEN_EXPIRED tokenFresh,
      check APP_NOT_RECOGNIZED appRecognized,
      check CERT_MISMATCH certMatches,
      check DEVICE_INTEGRITY_LOW deviceVerdictMet,
      check APP_UNLICENSED (not requireLicensedApp || licensingVerdict == Just "LICENSED"),
      check APP_ACCESS_RISK (not $ any (`elem` rejectedAppAccessRisks) detectedRisks),
      check DEVICE_ACTIVITY_HIGH activityAcceptable
    ]
  where
    check reason ok = if ok then Nothing else Just reason

    requestPackage = payload.requestDetails >>= (.requestPackageName)
    appPackage = payload.appIntegrity >>= (.packageName)
    appVerdict = payload.appIntegrity >>= (.appRecognitionVerdict)
    licensingVerdict = payload.accountDetails >>= (.appLicensingVerdict)
    detectedRisks = fromMaybe [] $ payload.environmentDetails >>= (.appAccessRiskVerdict) >>= (.appsDetected)
    deviceVerdicts = fromMaybe [] $ payload.deviceIntegrity >>= (.deviceRecognitionVerdict)
    presentedDigests = maybe [] (map normalizeCertDigest) $ payload.appIntegrity >>= (.certificateSha256Digest)

    appRecognized =
      appVerdict == Just "PLAY_RECOGNIZED"
        || (not requirePlayRecognizedApp && appVerdict == Just "UNRECOGNIZED_VERSION")

    matchedApps = filter (\allowed -> Just allowed.packageName == requestPackage) allowedApps
    packageMatches = not (null matchedApps) && appPackage == requestPackage

    requestHashMatches =
      (stripBase64Padding <$> (payload.requestDetails >>= (.requestHash))) == Just (stripBase64Padding expectedRequestHash)

    tokenFresh = case payload.requestDetails >>= (.timestampMillis) of
      Nothing -> False
      Just (FlexibleInt issuedAtMillis) ->
        let nowMillis = round (utcTimeToPOSIXSeconds now * 1000) :: Integer
         in abs (nowMillis - issuedAtMillis) <= fromIntegral tokenMaxAgeSeconds * 1000

    certMatches = any (\allowed -> any (`elem` map normalizeCertDigest allowed.certSha256Digests) presentedDigests) matchedApps

    deviceVerdictMet = any (`elem` deviceVerdicts) acceptedDeviceVerdicts
    acceptedDeviceVerdicts = map (T.pack . show) [minDeviceVerdict .. maxBound]

    activityAcceptable = case maxRecentDeviceActivityLevel of
      Nothing -> True
      Just maxLevel -> case payload.deviceIntegrity >>= (.recentDeviceActivity) >>= (.deviceActivityLevel) >>= activityLevel of
        Nothing -> True
        Just level -> level <= maxLevel

activityLevel :: Text -> Maybe Int
activityLevel raw = T.stripPrefix "LEVEL_" raw >>= readMaybe . T.unpack

normalizeCertDigest :: Text -> Text
normalizeCertDigest raw =
  let stripped = T.filter (/= ':') $ T.strip raw
   in if T.length stripped == 64 && T.all isHexDigit stripped
        then encodeBase64Url . BS.pack . hexBytes $ T.unpack stripped
        else stripBase64Padding $ T.strip raw
  where
    hexBytes (a : b : rest) = fromIntegral (digitToInt a * 16 + digitToInt b) : hexBytes rest
    hexBytes _ = []
