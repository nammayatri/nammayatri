module App.Routes
  ( MockSmsAPI,
    mockSmsAPI,
    mockSmsServer,
  )
where

import App.Types
import EulerHS.Prelude
import qualified Kernel.External.SMS.MyValueFirst.API as API
import Kernel.Types.App (FlowServerR)
import qualified Product.Sms as P
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
