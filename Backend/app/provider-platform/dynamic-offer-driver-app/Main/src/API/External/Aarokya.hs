module API.External.Aarokya where

import Domain.Types.External.Aarokya
import EulerHS.Types as Euler
import Kernel.Prelude
import Servant (Header, JSON, Post, ReqBody, (:>))

type AarokyaGenerateTokenAPI =
  "auth"
    :> "token"
    :> Header "api-key" Text
    :> ReqBody '[JSON] AarokyaTokenRequest
    :> Post '[JSON] AarokyaTokenResponse

aarokyaGenerateTokenAPI :: Proxy AarokyaGenerateTokenAPI
aarokyaGenerateTokenAPI = Proxy

generateToken :: Maybe Text -> AarokyaTokenRequest -> Euler.EulerClient AarokyaTokenResponse
generateToken = Euler.client aarokyaGenerateTokenAPI
