module App.Routes
  ( MockFcmAPI,
    mockFcmAPI,
    mockFcmServer,
  )
where

import App.Types
import Data.Aeson
import EulerHS.Prelude
import Kernel.External.FCM.Flow as API
import Kernel.Types.App (FlowServerR)
import qualified Product.Fcm as P
import Servant hiding (throwError)
import Types.API.Fcm as API

type MockFcmAPI =
  Get '[JSON] Text
    :<|> API.FCMSendMessageAPI Value
    :<|> API.ReadFcmAPI

mockFcmAPI :: Proxy MockFcmAPI
mockFcmAPI = Proxy

mockFcmServer :: FlowServerR AppEnv MockFcmAPI
mockFcmServer =
  pure "MockFcm is UP"
    :<|> P.sendFcm
    :<|> P.readFcm
