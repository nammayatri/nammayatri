{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.AppManagement.Overlay
  ( postOverlayCreate,
    postOverlayDelete,
    getOverlayList,
    getOverlayInfo,
    postOverlaySchedule,
  )
where

import qualified API.Types.Dashboard.AppManagement.Overlay as DAO
import Data.Ord
import Data.Time hiding (getCurrentTime)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Overlay as DTMO
import Environment
import Kernel.External.Types (Language (..))
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Id as ID
import Kernel.Utils.Common
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.Beam.SchedulerJob ()
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import qualified Storage.Queries.Overlay as SQMO
import Tools.Error

-- =============================================
------------- create overlay -------------------

postOverlayCreate :: ShortId DM.Merchant -> Context.City -> DAO.CreateOverlayReq -> Flow APISuccess
postOverlayCreate merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  overlayPresent <- SQMO.findAllByOverlayKeyUdfVehicleCategory merchantOpCityId req.overlayKey req.udf1 req.vehicleCategory
  unless (null overlayPresent) $ throwError $ OverlayKeyAndUdfAlreadyPresent ("overlayKey : " <> req.overlayKey <> " and " <> "udf : " <> show req.udf1)
  overlays <- mapM (buildOverlay merchant.id merchantOpCityId req) req.contents
  SQMO.createMany overlays
  pure Success
  where
    buildOverlay :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DAO.CreateOverlayReq -> DAO.OverlayContent -> Flow DTMO.Overlay
    buildOverlay merchantId merchantOpCityId _overlay@DAO.CreateOverlayReq {..} _content@DAO.OverlayContent {..} = do
      guid <- ID.Id <$> generateGUID
      return
        DTMO.Overlay
          { id = guid,
            merchantId,
            merchantOperatingCityId = merchantOpCityId,
            actions2 = fromMaybe [] actions2,
            vehicleCategory = vehicleCategory,
            ..
          }

-- =============================================
------------- delete overlay -------------------

availableLanguages :: [Language]
availableLanguages = [ENGLISH, HINDI, KANNADA, TAMIL, MALAYALAM, BENGALI, FRENCH]

postOverlayDelete :: ShortId DM.Merchant -> Context.City -> DAO.DeleteOverlayReq -> Flow APISuccess
postOverlayDelete merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  overlayPresent <- SQMO.findAllByOverlayKeyUdfVehicleCategory merchantOpCityId req.overlayKey req.udf1 req.vehicleCategory
  when (null overlayPresent) $ throwError $ OverlayKeyAndUdfNotFound ("overlayKey : " <> req.overlayKey <> " and " <> "udf : " <> show req.udf1)
  SQMO.deleteByOverlayKeyMerchantOpCityIdUdf merchantOpCityId req.overlayKey req.udf1
  mapM_ (\language -> CMP.clearMerchantIdPNKeyLangaugeUdf merchantOpCityId req.overlayKey language req.udf1 req.vehicleCategory) availableLanguages
  pure Success

-- ============================================
-------------- list overlay -------------------

getOverlayList :: ShortId DM.Merchant -> Context.City -> Flow DAO.ListOverlayResp
getOverlayList merchantShortId opCity = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  SQMO.findAllByLanguage merchantOpCityId ENGLISH >>= mapM buildListOverlayResp
  where
    buildListOverlayResp _overlay@DTMO.Overlay {..} = do
      return DAO.OverlayItem {..}

-- ============================================
-------------- overlay Info -------------------

getOverlayInfo :: ShortId DM.Merchant -> Context.City -> Maybe Text -> Text -> Flow DAO.OverlayInfoResp
getOverlayInfo merchantShortId opCity udf1Req overlayReqKey = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  overlays <- SQMO.findAllByOverlayKeyUdf merchantOpCityId overlayReqKey udf1Req
  buildOverlayInfoResp overlays
  where
    buildOverlayInfoResp overlays = do
      if null overlays
        then do throwError $ OverlayKeyAndUdfNotFound ("overlayKey : " <> overlayReqKey <> " and " <> "udf : " <> show udf1Req)
        else do
          let _res@DTMO.Overlay {..} = minimumBy (comparing (.language)) overlays
          groupedContents <- mapM buildGroupedContents overlays
          return
            DAO.CreateOverlayReq
              { contents = groupedContents,
                actions2 = Just actions2,
                vehicleCategory = vehicleCategory,
                ..
              }

    buildGroupedContents _overlay@DTMO.Overlay {..} = do
      return DAO.OverlayContent {..}

-- ==================================================
---------------- schedule Overlay -------------------

postOverlaySchedule :: ShortId DM.Merchant -> Context.City -> DAO.ScheduleOverlay -> Flow APISuccess
postOverlaySchedule merchantShortId opCity req@DAO.ScheduleOverlay {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let scheduledTime = UTCTime (utctDay now) (timeOfDayToTime req.scheduleTime)
  let jobScheduledTime =
        if scheduledTime < now
          then diffUTCTime (addUTCTime 86400 (UTCTime (utctDay now) (timeOfDayToTime req.scheduleTime))) now
          else diffUTCTime (UTCTime (utctDay now) (timeOfDayToTime req.scheduleTime)) now
  createJobIn @_ @'SendOverlay (Just merchant.id) (Just merchantOpCityId) jobScheduledTime $
    SendOverlayJobData
      { merchantId = merchant.id,
        rescheduleInterval = req.rescheduleInterval,
        scheduledTime = req.scheduleTime,
        freeTrialDays = transporterConfig.freeTrialDays,
        timeDiffFromUtc = transporterConfig.timeDiffFromUtc,
        driverPaymentCycleDuration = transporterConfig.driverPaymentCycleDuration,
        driverPaymentCycleStartTime = transporterConfig.driverPaymentCycleStartTime,
        driverFeeOverlaySendingTimeLimitInDays = transporterConfig.driverFeeOverlaySendingTimeLimitInDays,
        overlayBatchSize = transporterConfig.overlayBatchSize,
        serviceName = Nothing,
        merchantOperatingCityId = Just merchantOpCityId,
        vehicleCategory = vehicleCategory,
        ..
      }
  pure Success
