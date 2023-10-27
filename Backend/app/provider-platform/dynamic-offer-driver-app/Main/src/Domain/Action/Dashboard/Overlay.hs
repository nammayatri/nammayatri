{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Domain.Action.Dashboard.Overlay where

import "dashboard-helper-api" Dashboard.Common (HideSecrets (hideSecrets))
import Data.Ord
import Data.Time hiding (getCurrentTime)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Merchant.Overlay as DTMO
import Environment
import Kernel.External.Types (Language (..))
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import qualified Storage.CachedQueries.Merchant.TransporterConfig as CQTC
import qualified Storage.Queries.Merchant.Overlay as SQMO
import Tools.Error

-- =============================================
------------- create overlay -------------------
data CreateOverlayReq = CreateOverlayReq
  { overlayKey :: Text,
    udf1 :: Maybe Text,
    actions :: [Text],
    link :: Maybe Text,
    imageUrl :: Maybe Text,
    reqBody :: Value,
    method :: Maybe Text,
    endPoint :: Maybe Text,
    contents :: [OverlayContent]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OverlayContent = OverlayContent
  { language :: Language,
    title :: Maybe Text,
    description :: Maybe Text,
    okButtonText :: Maybe Text,
    cancelButtonText :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets CreateOverlayReq where
  hideSecrets = identity

createOverlay :: ShortId DM.Merchant -> Context.City -> CreateOverlayReq -> Flow APISuccess
createOverlay merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  overlayPresent <- SQMO.findAllByOverlayKeyUdf merchantOpCityId req.overlayKey req.udf1
  unless (null overlayPresent) $ throwError $ OverlayKeyAndUdfAlreadyPresent ("overlayKey : " <> req.overlayKey <> " and " <> "udf : " <> show req.udf1)
  overlays <- mapM (buildOverlay merchant.id merchantOpCityId req) req.contents
  SQMO.createMany overlays
  pure Success
  where
    buildOverlay :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> CreateOverlayReq -> OverlayContent -> Flow DTMO.Overlay
    buildOverlay merchantId merchantOpCityId _overlay@CreateOverlayReq {..} _content@OverlayContent {..} = do
      guid <- Id <$> generateGUID
      return
        DTMO.Overlay
          { id = guid,
            merchantId,
            merchantOperatingCityId = merchantOpCityId,
            ..
          }

-- =============================================
------------- delete overlay -------------------

data DeleteOverlayReq = DeleteOverlayReq
  { overlayKey :: Text,
    udf1 :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets DeleteOverlayReq where
  hideSecrets = identity

availableLanguages :: [Language]
availableLanguages = [ENGLISH, HINDI, KANNADA, TAMIL, MALAYALAM, BENGALI, FRENCH]

deleteOverlay :: ShortId DM.Merchant -> Context.City -> DeleteOverlayReq -> Flow APISuccess
deleteOverlay merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  overlayPresent <- SQMO.findAllByOverlayKeyUdf merchantOpCityId req.overlayKey req.udf1
  when (null overlayPresent) $ throwError $ OverlayKeyAndUdfNotFound ("overlayKey : " <> req.overlayKey <> " and " <> "udf : " <> show req.udf1)
  SQMO.deleteByOverlayKeyMerchantIdUdf merchantOpCityId req.overlayKey req.udf1
  mapM_ (\language -> CMP.clearMerchantIdPNKeyLangaugeUdf merchantOpCityId req.overlayKey language req.udf1) availableLanguages
  pure Success

-- ============================================
-------------- list overlay -------------------

data OverlayItem = OverlayItem
  { overlayKey :: Text,
    udf1 :: Maybe Text,
    actions :: [Text],
    link :: Maybe Text,
    imageUrl :: Maybe Text,
    title :: Maybe Text,
    description :: Maybe Text,
    okButtonText :: Maybe Text,
    cancelButtonText :: Maybe Text,
    reqBody :: Value,
    method :: Maybe Text,
    endPoint :: Maybe Text
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets OverlayItem where
  hideSecrets = identity

type ListOverlayResp = [OverlayItem]

listOverlay :: ShortId DM.Merchant -> Context.City -> Flow ListOverlayResp
listOverlay merchantShortId opCity = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  SQMO.findAllByLanguage merchantOpCityId ENGLISH >>= mapM buildListOverlayResp
  where
    buildListOverlayResp _overlay@DTMO.Overlay {..} = do
      return OverlayItem {..}

-- ============================================
-------------- overlay Info -------------------

type OverlayInfoResp = CreateOverlayReq

data OverlayInfoReq = OverlayInfoReq
  { overlayKey :: Text,
    udf1 :: Maybe Text
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets OverlayInfoReq where
  hideSecrets = identity

overlayInfo :: ShortId DM.Merchant -> Context.City -> OverlayInfoReq -> Flow OverlayInfoResp
overlayInfo merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  overlays <- SQMO.findAllByOverlayKeyUdf merchantOpCityId req.overlayKey req.udf1
  buildOverlayInfoResp overlays
  where
    buildOverlayInfoResp overlays = do
      if null overlays
        then do throwError $ OverlayKeyAndUdfNotFound ("overlayKey : " <> req.overlayKey <> " and " <> "udf : " <> show req.udf1)
        else do
          let _res@DTMO.Overlay {..} = minimumBy (comparing (.language)) overlays
          groupedContents <- mapM buildGroupedContents overlays
          return
            CreateOverlayReq
              { contents = groupedContents,
                ..
              }

    buildGroupedContents _overlay@DTMO.Overlay {..} = do
      return OverlayContent {..}

-- ==================================================
---------------- schedule Overlay -------------------

data ScheduleOverlay = ScheduleOverlay
  { scheduleTime :: TimeOfDay,
    rescheduleInterval :: Maybe Seconds,
    condition :: DTMO.OverlayCondition,
    overlayKey :: Text,
    udf1 :: Maybe Text
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets ScheduleOverlay where
  hideSecrets = identity

scheduleOverlay :: ShortId DM.Merchant -> Context.City -> ScheduleOverlay -> Flow APISuccess
scheduleOverlay merchantShortId opCity req@ScheduleOverlay {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  maxShards <- asks (.maxShards)
  transporterConfig <- CQTC.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let scheduledTime = UTCTime (utctDay now) (timeOfDayToTime req.scheduleTime)
  let jobScheduledTime =
        if scheduledTime < now
          then diffUTCTime (addUTCTime 86400 (UTCTime (utctDay now) (timeOfDayToTime req.scheduleTime))) now
          else diffUTCTime (UTCTime (utctDay now) (timeOfDayToTime req.scheduleTime)) now
  createJobIn @_ @'SendOverlay jobScheduledTime maxShards $
    SendOverlayJobData
      { merchantId = merchant.id,
        rescheduleInterval = req.rescheduleInterval,
        scheduledTime = req.scheduleTime,
        freeTrialDays = transporterConfig.freeTrialDays,
        timeDiffFromUtc = transporterConfig.timeDiffFromUtc,
        driverPaymentCycleDuration = transporterConfig.driverPaymentCycleDuration,
        driverPaymentCycleStartTime = transporterConfig.driverPaymentCycleStartTime,
        ..
      }
  pure Success
