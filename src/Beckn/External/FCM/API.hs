module Beckn.External.FCM.API where

import           EulerHS.Prelude

import           Beckn.External.FCM.Types
import           Beckn.Types.App          (MandatoryQueryParam)
import qualified EulerHS.Types            as ET
import           Servant
import           Servant.API.ContentTypes
import           Servant.Client

type ServiceAPI
   = Header "Authorization" FAuth
    :> "v1"
    :> "projects"
    :> Capture "projectId" Text
    :> "messages"
    :> ReqBody '[ JSON] SubmitNotification
    :> Post '[ JSON] SubmitNotificationResp

serviceAPI :: Proxy ServiceAPI
serviceAPI = Proxy

sendNotification header param req =
  void $ ET.client serviceAPI header param req
