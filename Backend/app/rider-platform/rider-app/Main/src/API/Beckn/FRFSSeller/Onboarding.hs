{-# LANGUAGE OverloadedStrings #-}

-- | The two endpoints the ONDC registry calls to admit a subscriber.
--
-- Neither is Beckn-signed: they run before we are a subscriber, so there is no key for
-- a buyer to verify us with yet. on_subscribe proves we hold the encryption key we
-- registered; the site-verification page proves we control the domain.
--
-- Path shape matches the Go service byte for byte, because the registry stores it.
module API.Beckn.FRFSSeller.Onboarding (API, handler) where

import qualified Data.ByteString.Char8 as C8
import qualified Domain.Action.Beckn.FRFSSeller.Onboarding as DOnboarding
import Environment
import Kernel.Prelude
import Kernel.Utils.Common
import Kernel.Utils.Servant.HTML (HTML)
import Servant hiding (throwError)

type API =
  Capture "operator" Text
    :> ( "onboarding"
           :> "on_subscribe"
           :> ReqBody '[JSON] DOnboarding.SubscribeReq
           :> Post '[JSON] DOnboarding.SubscribeRes
           :<|> "ondc-site-verification.html"
             :> QueryParam "request_id" Text
             :> Get '[HTML] C8.ByteString
       )

handler :: FlowServer API
handler operator = onSubscribe operator :<|> siteVerification operator

onSubscribe :: Text -> DOnboarding.SubscribeReq -> FlowHandler DOnboarding.SubscribeRes
onSubscribe operator req = withFlowHandlerAPI $ DOnboarding.onSubscribe operator req

siteVerification :: Text -> Maybe Text -> FlowHandler C8.ByteString
siteVerification operator mbRequestId = withFlowHandlerAPI $ DOnboarding.siteVerification operator mbRequestId
