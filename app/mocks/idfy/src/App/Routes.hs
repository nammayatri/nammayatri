module App.Routes where

import App.Types
import Beckn.Types.App (FlowServerR)
import EulerHS.Prelude
import Idfy.External.Flow
import qualified Product.Idfy as P
import Servant hiding (throwError)

type MockIdfyAPI =
  VerifyDLAPI
    :<|> VerifyRCAPI
    :<|> ValidateImage
    :<|> ExtractDLImage
    :<|> ExtractRCAPI

mockIdfyAPI :: Proxy MockIdfyAPI
mockIdfyAPI = Proxy

mockIdfyServer :: FlowServerR AppEnv MockIdfyAPI
mockIdfyServer =
  P.verifyDL
    :<|> P.verifyRC
    :<|> P.validateImage
    :<|> P.extractDLImage
    :<|> P.extractRCImage
