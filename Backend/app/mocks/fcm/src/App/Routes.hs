{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module App.Routes
  ( MockFcmAPI,
    mockFcmAPI,
    mockFcmServer,
  )
where

import App.Types
import Data.Aeson
import EulerHS.Prelude
import Kernel.External.Notification.FCM.Flow as API
import Kernel.Types.App (FlowServerR)
import qualified Product.Fcm as P
import Servant hiding (throwError)
import Types.API.Fcm as API

type MockFcmAPI =
  Get '[JSON] Text
    :<|> API.FCMSendMessageAPI Value Value
    :<|> API.ReadFcmAPI

mockFcmAPI :: Proxy MockFcmAPI
mockFcmAPI = Proxy

mockFcmServer :: FlowServerR AppEnv MockFcmAPI
mockFcmServer =
  pure "MockFcm is UP"
    :<|> P.sendFcm
    :<|> P.readFcm
