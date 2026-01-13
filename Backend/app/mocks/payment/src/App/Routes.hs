module App.Routes
  ( MockPaymentAPI,
    mockPaymentAPI,
    mockPaymentServer,
  )
where

import API (API)
import App.Types
import EulerHS.Prelude
import qualified Handler
import Kernel.Types.App (FlowServerR)
import Servant hiding (throwError)

type MockPaymentAPI =
  Get '[JSON] Text
    :<|> API

mockPaymentAPI :: Proxy MockPaymentAPI
mockPaymentAPI = Proxy

mockPaymentServer :: FlowServerR AppEnv MockPaymentAPI
mockPaymentServer =
  pure "MockPayment is UP"
    :<|> Handler.server
