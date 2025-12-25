{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.Overlay where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Domain.Types.Overlay
import qualified Domain.Types.VehicleCategory
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Notification.FCM.Types
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import Servant
import Servant.Client

data CreateOverlayReq = CreateOverlayReq
  { overlayKey :: Kernel.Prelude.Text,
    udf1 :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    actions :: [Kernel.Prelude.Text],
    link :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    imageUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    reqBody :: Data.Aeson.Value,
    method :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    endPoint :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    contents :: [OverlayContent],
    delay :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    contactSupportNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    toastMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    secondaryActions :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    actions2 :: Kernel.Prelude.Maybe [Kernel.External.Notification.FCM.Types.FCMActions],
    socialMediaLinks :: Kernel.Prelude.Maybe [Kernel.External.Notification.FCM.Types.FCMMediaLink],
    secondaryActions2 :: Kernel.Prelude.Maybe [Kernel.External.Notification.FCM.Types.FCMActions],
    showPushNotification :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CreateOverlayReq where
  hideSecrets = Kernel.Prelude.identity

data DeleteOverlayReq = DeleteOverlayReq {overlayKey :: Kernel.Prelude.Text, udf1 :: Kernel.Prelude.Maybe Kernel.Prelude.Text, vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets DeleteOverlayReq where
  hideSecrets = Kernel.Prelude.identity

type ListOverlayResp = [OverlayItem]

data OverlayContent = OverlayContent
  { language :: Kernel.External.Types.Language,
    title :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    okButtonText :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    cancelButtonText :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets OverlayContent where
  hideSecrets = Kernel.Prelude.identity

type OverlayInfoResp = CreateOverlayReq

data OverlayItem = OverlayItem
  { overlayKey :: Kernel.Prelude.Text,
    udf1 :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    actions :: [Kernel.Prelude.Text],
    link :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    imageUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    title :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    okButtonText :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    cancelButtonText :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    reqBody :: Data.Aeson.Value,
    method :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    endPoint :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    delay :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    contactSupportNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    toastMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    secondaryActions :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    socialMediaLinks :: Kernel.Prelude.Maybe [Kernel.External.Notification.FCM.Types.FCMMediaLink],
    showPushNotification :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets OverlayItem where
  hideSecrets = Kernel.Prelude.identity

data ScheduleOverlay = ScheduleOverlay
  { scheduleTime :: Kernel.Prelude.TimeOfDay,
    rescheduleInterval :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    condition :: Domain.Types.Overlay.OverlayCondition,
    overlayKey :: Kernel.Prelude.Text,
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory,
    udf1 :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets ScheduleOverlay where
  hideSecrets = Kernel.Prelude.identity

type API = ("overlay" :> (PostOverlayCreate :<|> PostOverlayDelete :<|> GetOverlayList :<|> GetOverlayInfo :<|> PostOverlaySchedule))

type PostOverlayCreate = ("createOverlay" :> ReqBody '[JSON] CreateOverlayReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostOverlayDelete = ("deleteOverlay" :> ReqBody '[JSON] DeleteOverlayReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type GetOverlayList = ("listOverlay" :> Get '[JSON] ListOverlayResp)

type GetOverlayInfo = ("overlayInfo" :> QueryParam "udf1" Kernel.Prelude.Text :> MandatoryQueryParam "overlayKey" Kernel.Prelude.Text :> Get '[JSON] OverlayInfoResp)

type PostOverlaySchedule = ("scheduleOverlay" :> ReqBody '[JSON] ScheduleOverlay :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

data OverlayAPIs = OverlayAPIs
  { postOverlayCreate :: CreateOverlayReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postOverlayDelete :: DeleteOverlayReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getOverlayList :: EulerHS.Types.EulerClient ListOverlayResp,
    getOverlayInfo :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient OverlayInfoResp,
    postOverlaySchedule :: ScheduleOverlay -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkOverlayAPIs :: (Client EulerHS.Types.EulerClient API -> OverlayAPIs)
mkOverlayAPIs overlayClient = (OverlayAPIs {..})
  where
    postOverlayCreate :<|> postOverlayDelete :<|> getOverlayList :<|> getOverlayInfo :<|> postOverlaySchedule = overlayClient

data OverlayUserActionType
  = POST_OVERLAY_CREATE
  | POST_OVERLAY_DELETE
  | GET_OVERLAY_LIST
  | GET_OVERLAY_INFO
  | POST_OVERLAY_SCHEDULE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''OverlayUserActionType])
