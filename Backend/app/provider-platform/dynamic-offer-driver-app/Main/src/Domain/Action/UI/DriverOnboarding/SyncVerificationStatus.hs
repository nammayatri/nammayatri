{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Pull-based recovery for stuck doc verifications: when a provider webhook is dropped, re-pull the
-- result via @getTask@ and apply the same @onVerify*@ handler (forked from the status flows).
-- DL/PAN/GST/UDYAM dispatch lives in @…DriverOnboarding.Status@ (their @onVerify*@ transitively
-- import this module, so dispatching them here would cycle); RC is cycle-free.
module Domain.Action.UI.DriverOnboarding.SyncVerificationStatus
  ( reconcilePending,
    pullRcStatus,
    isPullableStatus,
    PullSources (..),
    pullSourcesFor,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Map.Strict as Map
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DRC
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.MerchantServiceUsageConfig as DMSUC
import qualified Domain.Types.Person as Person
import Environment (Flow)
import qualified Kernel.External.Verification as KEV
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified SharedLogic.DriverOnboarding as SDO
import SharedLogic.DriverOnboarding.VehicleDocs (ResponseStatus (..))
import Storage.ConfigPilot.Config.MerchantServiceUsageConfig (MerchantServiceUsageConfigDimensions (..))
import qualified Storage.Queries.HyperVergeVerification as HVQuery
import qualified Storage.Queries.HyperVergeVerificationExtra as HVQueryExtra
import qualified Storage.Queries.IdfyVerification as IVQuery
import qualified Storage.Queries.IdfyVerificationExtra as IVQueryExtra
import Tools.Error
import qualified Tools.Verification as Verification

-- | Statuses that can carry a still-pending verification row. Total match, no wildcard — with -Werror
--   a new status fails compilation here and must decide its pullability explicitly.
isPullableStatus :: ResponseStatus -> Bool
isPullableStatus = \case
  PENDING -> True
  -- Strict PAN/GST write this placeholder while their verification row is still pending.
  MANUAL_VERIFICATION_REQUIRED -> True
  PULL_REQUIRED -> True
  VALID -> False
  NO_DOC_AVAILABLE -> False
  -- Rendered only when the latest row is NOT pending.
  FAILED -> False
  -- Re-verify paths move entities off INVALID before creating a pending row; what remains is an ops rejection.
  INVALID -> False
  LIMIT_EXCEED -> False
  UNAUTHORIZED -> False
  CONSENT_DENIED -> False

-- | Webhook head start: rows younger than this are left for the webhook to resolve.
webhookGracePeriod :: NominalDiffTime
webhookGracePeriod = 120 -- 2 minutes

-- | Which provider tables can hold a pending row for a doc under the CURRENT config — an upper bound,
--   ORed across driver-app + dashboard columns (a row doesn't record its surface). PAN/GST/UDYAM
--   writers are Idfy-or-throw; DL adds HV for the SDK path; RC unions every list its dispatch can read.
--   Trade-off: rows created under a previous config stop being pulled after a provider flip.
data PullSources = PullSources
  { checkIdfy :: Bool,
    checkHyperVerge :: Bool
  }

pullSourcesFor :: DMSUC.MerchantServiceUsageConfig -> DVC.DocumentType -> PullSources
pullSourcesFor cfg docType = case docType of
  DVC.PanCard -> idfyGated [cfg.panVerificationService, cfg.dashboardPanVerificationService]
  DVC.GSTCertificate -> idfyGated [cfg.gstVerificationService, cfg.dashboardGstVerificationService]
  DVC.UDYAMCertificate -> PullSources {checkIdfy = KEV.Idfy `elem` cfg.verificationProvidersPriorityList, checkHyperVerge = False}
  DVC.DriverLicense ->
    PullSources
      { checkIdfy = KEV.Idfy `elem` cfg.verificationProvidersPriorityList,
        checkHyperVerge = KEV.HyperVergeRCDL `elem` cfg.verificationProvidersPriorityList || cfg.sdkVerificationService == KEV.HyperVerge
      }
  DVC.VehicleRegistrationCertificate ->
    let rcProviders =
          concat (maybe [] Map.elems cfg.categoryBasedVerificationPriorityList)
            <> fromMaybe [] cfg.totoVerificationPriorityList
            <> cfg.verificationProvidersPriorityList
            <> [KEV.Idfy]
     in PullSources {checkIdfy = KEV.Idfy `elem` rcProviders, checkHyperVerge = KEV.HyperVergeRCDL `elem` rcProviders}
  _ -> PullSources {checkIdfy = True, checkHyperVerge = True}
  where
    idfyGated gates = PullSources {checkIdfy = Just KEV.Idfy `elem` gates, checkHyperVerge = False}

-- | One provider's newest still-pending row for a doc, as the shared 'SDO.VerificationReqRecord' + provider tag.
data PendingRow = PendingRow
  { rowCreatedAt :: UTCTime,
    rowService :: KEV.VerificationService,
    rowReq :: SDO.VerificationReqRecord
  }

-- | Throttle to one attempt per doc per window, read only the tables 'pullSourcesFor' allows, and
--   re-pull the newest still-pending row via getTask, handing it to @dispatch@ (the @onVerify*@; a
--   callback to avoid an import cycle). 'withTryCatch' logs provider errors instead of propagating.
reconcilePending ::
  Person.Person ->
  DVC.DocumentType ->
  Maybe Text ->
  (SDO.VerificationReqRecord -> KEV.GetTaskResp -> KEV.VerificationService -> Flow ()) ->
  Flow ()
reconcilePending person docType mbImageIdTxt dispatch = do
  -- Throttle before any lookup: a stuck doc costs one Redis op per render; reads + getTask run at most
  -- once per window. A row still in the grace window consumes a window, so it heals one window later.
  firstPullInWindow <- Redis.setNxExpire pullThrottleKey (round webhookGracePeriod) ()
  when firstPullInWindow $ do
    merchantServiceUsageConfig <-
      getOneConfig (MerchantServiceUsageConfigDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId}) Nothing
        >>= fromMaybeM (MerchantServiceUsageConfigNotFound person.merchantOperatingCityId.getId)
    let sources = pullSourcesFor merchantServiceUsageConfig docType
    if not sources.checkIdfy && not sources.checkHyperVerge
      then logDebug $ "reconcilePending: no async provider configured for " <> show docType <> " in city " <> person.merchantOperatingCityId.getId <> ", skipping pull"
      else do
        mbIdfyRow <- if sources.checkIdfy then findLatestIdfyPending else pure Nothing
        mbHVRow <- if sources.checkHyperVerge then findLatestHVPending else pure Nothing
        let idfyRow = mbIdfyRow >>= \r -> PendingRow r.createdAt KEV.Idfy <$> SDO.makeIdfyVerificationReqRecord r
            hvRow = mbHVRow <&> \r -> PendingRow r.createdAt KEV.HyperVergeRCDL (SDO.makeHVVerificationReqRecord r)
        -- Surface an Idfy row that failed to convert — otherwise it's dropped silently and stays stuck.
        when (isJust mbIdfyRow && isNothing idfyRow) $
          logWarning $ "reconcilePending: dropped a pending Idfy row for " <> show docType <> " (could not build VerificationReqRecord)"
        now <- getCurrentTime
        whenJust (newer idfyRow hvRow) $ \row ->
          -- Skip rows still within the webhook grace window — a fresh row is the webhook's to resolve.
          when (diffUTCTime now row.rowCreatedAt > webhookGracePeriod) $ reconcileRow row
  where
    pullThrottleKey = "verifyPull:" <> person.id.getId <> ":" <> show docType <> maybe "" (":" <>) mbImageIdTxt

    -- Image-keyed (RC / fleet: disambiguate this vehicle's row from the driver's other RCs) vs
    -- driver-keyed (one-per-driver docs: DL / PAN / GST / UDYAM).
    findLatestIdfyPending = case mbImageIdTxt of
      Just imgTxt -> IVQueryExtra.findLatestPendingByDocTypeAndImage person.id docType (Id imgTxt)
      Nothing -> IVQueryExtra.findLatestPendingByDriverIdAndDocType person.id docType

    findLatestHVPending = case mbImageIdTxt of
      Just imgTxt -> HVQueryExtra.findLatestPendingByDocTypeAndImage person.id docType (Id imgTxt)
      Nothing -> HVQueryExtra.findLatestPendingByDriverIdAndDocType person.id docType

    -- The priority list can fall back across providers for the same doc, so the newer row wins.
    newer (Just a) (Just b) = Just $ if a.rowCreatedAt >= b.rowCreatedAt then a else b
    newer a b = a <|> b

    reconcileRow PendingRow {rowService, rowReq} =
      void . withTryCatch ("reconcilePending:" <> rowReq.requestId) $ do
        -- Cross-path dedupe: the sync DL backstop may have pulled this requestId moments ago.
        firstReqPull <- Redis.setNxExpire (SDO.getTaskPullKey rowReq.requestId) (round SDO.getTaskPullWindow) ()
        when firstReqPull $ do
          allowed <- SDO.allowGetTaskAttempt rowReq.requestId
          if not allowed
            then logInfo $ "reconcilePending: attempt cap hit for " <> rowReq.requestId <> ", cooling down"
            else do
              let getTaskReq = KEV.GetTaskReq (hvWorkflowHint rowService) rowReq.requestId
              resp <- Verification.getTask person.merchantOperatingCityId rowService getTaskReq (updateResponseFor rowService)
              dispatch rowReq resp rowService

    -- HyperVerge getVerificationStatus needs the workflow name (DL/RC only); Idfy ignores the hint.
    hvWorkflowHint KEV.HyperVergeRCDL = case docType of
      DVC.DriverLicense -> Just "checkDL"
      DVC.VehicleRegistrationCertificate -> Just "RCVerification"
      _ -> Nothing
    hvWorkflowHint _ = Nothing

    updateResponseFor KEV.HyperVergeRCDL = HVQuery.updateResponse
    updateResponseFor _ = IVQuery.updateResponse

-- | Re-pull RC status, keyed by THIS RC's image so a fleet driver's other RCs are untouched.
pullRcStatus :: Person.Person -> Text -> Flow ()
pullRcStatus person rcImageIdTxt =
  reconcilePending person DVC.VehicleRegistrationCertificate (Just rcImageIdTxt) (dispatchRc person)

dispatchRc :: Person.Person -> SDO.VerificationReqRecord -> KEV.GetTaskResp -> KEV.VerificationService -> Flow ()
dispatchRc person req resp service = case resp of
  KEV.RCResp o -> void $ DRC.onVerifyRC person (Just req) o Nothing Nothing Nothing req.documentImageId1 req.retryCount (Just req.status) (Just service) Nothing
  _ -> logInfo $ "pullRcStatus: unexpected response for " <> req.requestId
