{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module App.Routes
  ( MockSmsAPI,
    mockSmsAPI,
    mockSmsServer,
  )
where

import App.Types
import EulerHS.Prelude
import Kernel.Types.App (FlowServerR)
import qualified Product.Sms as P
import qualified SMS.MyValueFirst.API as API
import Servant hiding (throwError)
import qualified Types.API.Sms as API

type MockSmsAPI =
  Get '[JSON] Text
    :<|> API.ServiceAPI
    :<|> API.ReadSmsAPI

mockSmsAPI :: Proxy MockSmsAPI
mockSmsAPI = Proxy

mockSmsServer :: FlowServerR AppEnv MockSmsAPI
mockSmsServer =
  pure "MockSms is UP"
    :<|> P.sendSms
    :<|> P.readSms
