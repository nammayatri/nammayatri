module App.Routes where

import App.Types
import Beckn.Types.App (FlowServerR)
import EulerHS.Prelude
import qualified Product.Idfy as P
import Servant hiding (throwError)
import Types.API.VerifyDLAsync
import Types.API.VerifyRCAsync

type MockIdfyAPI =
  VerifyDLAPI
    :<|> VerifyRCAPI

mockIdfyAPI :: Proxy MockIdfyAPI
mockIdfyAPI = Proxy

mockIdfyServer :: FlowServerR AppEnv MockIdfyAPI
mockIdfyServer =
  P.verifyDL
    :<|> P.verifyRC
