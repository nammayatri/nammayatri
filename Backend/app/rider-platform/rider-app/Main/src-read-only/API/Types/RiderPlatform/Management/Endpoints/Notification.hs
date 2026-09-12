{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.Notification where

import qualified DashboardAlert.Domain.Action.Dashboard.List
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Domain.Types.Alert.AlertRequestStatus
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import Servant
import Servant.Client

data RespondReq = RespondReq {notificationId :: Kernel.Prelude.Text, status :: Domain.Types.Alert.AlertRequestStatus.AlertRequestStatus, reason :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets RespondReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("notification" :> (GetNotificationNotificationListHelper :<|> PostNotificationNotificationRespondHelper))

type GetNotificationNotificationList =
  ( "notification" :> "list" :> QueryParam "mbLimit" Kernel.Prelude.Int :> QueryParam "mbOffset" Kernel.Prelude.Int
      :> Get
           ('[JSON])
           DashboardAlert.Domain.Action.Dashboard.List.NotificationListResp
  )

type GetNotificationNotificationListHelper =
  ( "notification" :> Capture "topic" Kernel.Prelude.Text :> "list" :> QueryParam "mbLimit" Kernel.Prelude.Int
      :> QueryParam
           "mbOffset"
           Kernel.Prelude.Int
      :> Get ('[JSON]) DashboardAlert.Domain.Action.Dashboard.List.NotificationListResp
  )

type PostNotificationNotificationRespond = ("notification" :> "respond" :> ReqBody ('[JSON]) RespondReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type PostNotificationNotificationRespondHelper =
  ( "notification" :> Capture "topic" Kernel.Prelude.Text :> "respond" :> ReqBody ('[JSON]) RespondReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

data NotificationAPIs = NotificationAPIs
  { getNotificationNotificationList :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> EulerHS.Types.EulerClient DashboardAlert.Domain.Action.Dashboard.List.NotificationListResp),
    postNotificationNotificationRespond :: (Kernel.Prelude.Text -> RespondReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)
  }

mkNotificationAPIs :: (Client EulerHS.Types.EulerClient API -> NotificationAPIs)
mkNotificationAPIs notificationClient = (NotificationAPIs {..})
  where
    getNotificationNotificationList :<|> postNotificationNotificationRespond = notificationClient

data NotificationUserActionType
  = GET_NOTIFICATION_NOTIFICATION_LIST
  | POST_NOTIFICATION_NOTIFICATION_RESPOND
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''NotificationUserActionType)])
