{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module SharedLogic.Pass.Eligibility
  ( PassEligibilityData (..),
    PassEligibilityResult (..),
    mkPassEligibilityData,
    checkPassPurchaseEligibility,
  )
where

import qualified Data.Aeson as A
import Data.Default.Class
import qualified Data.Text as T
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Pass as DPass
import qualified Domain.Types.PassDetails as DPassDetails
import qualified Domain.Types.PassType as DPassType
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config (ClickhouseFlow)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id (Id, cast)
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Types as LYT

data PassEligibilityData = PassEligibilityData
  { passId :: Text,
    passCode :: Text,
    passEnum :: Maybe DPassType.PassEnum,
    personId :: Text,
    gender :: Text,
    dateOfBirth :: Maybe UTCTime,
    city :: Text,
    merchantOperatingCityId :: Text,
    hasDisability :: Maybe Bool,
    customerNammaTags :: Maybe [LYT.TagNameValueExpiry],
    aadhaarVerified :: Bool,
    enabled :: Bool,
    blocked :: Bool,
    verificationStatus :: Maybe DPassDetails.VerificationStatus,
    numberOfStages :: Maybe Int
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance Default PassEligibilityData where
  def =
    PassEligibilityData
      { passId = "",
        passCode = "",
        passEnum = Nothing,
        personId = "",
        gender = "",
        dateOfBirth = Nothing,
        city = "",
        merchantOperatingCityId = "",
        hasDisability = Nothing,
        customerNammaTags = Nothing,
        aadhaarVerified = False,
        enabled = True,
        blocked = False,
        verificationStatus = Nothing,
        numberOfStages = Nothing
      }

-- | Single source of truth for the rule input. Both the listing and the purchase
-- gate build it from here, so a field added on one path cannot silently go
-- missing on the other -- which would let a rule that rejects on the listing
-- pass at purchase.
mkPassEligibilityData ::
  DP.Person ->
  DPass.Pass ->
  DPassType.PassType ->
  Maybe DPassDetails.PassDetails ->
  PassEligibilityData
mkPassEligibilityData person pass passType mbPassDetails =
  PassEligibilityData
    { passId = pass.id.getId,
      passCode = pass.code,
      passEnum = passType.passEnum,
      personId = person.id.getId,
      gender = T.pack (show person.gender),
      dateOfBirth = person.dateOfBirth,
      city = T.pack (show person.currentCity),
      merchantOperatingCityId = person.merchantOperatingCityId.getId,
      hasDisability = person.hasDisability,
      customerNammaTags = person.customerNammaTags,
      aadhaarVerified = person.aadhaarVerified,
      enabled = person.enabled,
      blocked = person.blocked,
      verificationStatus = (.verificationStatus) <$> mbPassDetails,
      numberOfStages = (.numberOfStages) =<< mbPassDetails
    }

-- | 'reason' is forward-compat for surfacing "why not eligible" UX. Today's
-- rules emit a bare bool — the custom FromJSON accepts that, an object
-- @{eligible, reason}@, or @null@.
data PassEligibilityResult = PassEligibilityResult
  { eligible :: Bool,
    reason :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, ToSchema)

instance FromJSON PassEligibilityResult where
  parseJSON v = case v of
    A.Bool b -> pure $ PassEligibilityResult b Nothing
    A.Null -> pure $ PassEligibilityResult True Nothing
    A.Object _ -> A.genericParseJSON A.defaultOptions v
    other -> fail $ "PassEligibilityResult: unsupported value: " <> show other

-- | Open-on-empty (no rules -> eligible=True), closed-on-error.
--
-- Each element of the ruleset is an independent predicate, ANDed together --
-- the same semantics the per-pass @purchase_eligibility_json_logic@ column had.
-- Note this deliberately does NOT hand the whole list to 'runLogics': that folds,
-- feeding each logic the *previous* logic's output, so every element after the
-- first would be evaluated against a bare @true@/@false@ instead of the pass
-- data (and @{"var": ...}@ against a non-object silently yields null), quietly
-- turning the whole check into a no-op.
checkPassPurchaseEligibility ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    ClickhouseFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  [A.Value] ->
  PassEligibilityData ->
  m PassEligibilityResult
checkPassPurchaseEligibility merchantOpCityId allLogics d =
  foldM step (PassEligibilityResult True Nothing) allLogics
  where
    step acc _ | not acc.eligible = pure acc -- first rejection wins
    step _ logic = do
      eResp <-
        withTryCatch ("runLogics:PASS_PURCHASE_ELIGIBILITY:" <> d.passId) $
          LYDL.runLogicsWithDebugLog
            LYDL.Rider
            (cast merchantOpCityId)
            LYT.PASS_PURCHASE_ELIGIBILITY
            (Just d.passId)
            [logic]
            d
      case eResp of
        Left e -> do
          logError $ "PASS_PURCHASE_ELIGIBILITY runner failed - passId=" <> d.passId <> " - " <> show e
          pure $ failClosed "eligibility_check_failed"
        Right resp -> case A.fromJSON resp.result of
          A.Success r -> pure r
          A.Error err -> do
            logError $ "PASS_PURCHASE_ELIGIBILITY result parse failed - passId=" <> d.passId <> " - " <> T.pack err <> " - " <> show resp.result
            pure $ failClosed "eligibility_result_parse_failed"
    failClosed reason = PassEligibilityResult False (Just reason)
