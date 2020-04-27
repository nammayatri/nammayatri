module Beckn.App.Routes where

import Beckn.Types.API.PassApplication
import Beckn.Types.API.Registration
import Beckn.Types.API.Common
import Beckn.Types.App
import Data.Aeson
import EulerHS.Prelude
import Servant
import qualified Beckn.Data.Accessor as Accessor
import qualified Data.Vault.Lazy as V

type EPassAPIs =
  "v1" :>
    ( Get '[ JSON] Text
    :<|> RegistrationAPIs
    :<|> PassApplicationAPIs
    )

type RegistrationAPIs
   = "token" :>
      ( ReqBody '[ JSON] InitiateLoginReq :> Post '[ JSON] InitiateLoginRes
      :<|> Capture "tokenId" Text :> ReqBody '[ JSON] LoginReq :> Post '[ JSON] Value
      )

type PassApplicationAPIs
  = "passApplication" :> Header "registrationToken" Text :>
    ( ReqBody '[ JSON] CreatePassApplicationReq :> Post '[ JSON] PassApplicationRes
    :<|> ReqBody '[ JSON] ListPassApplicationReq :> Post '[ JSON] ListPassApplicationRes
    :<|> Capture "passApplicationId" Text :> Get '[JSON] PassApplicationRes
    )

epassAPIs :: Proxy EPassAPIs
epassAPIs = Proxy

healthCheckApp :: FlowHandler Text
healthCheckApp = pure "App is UP"

---- Registration Flow ------
initiateLogin :: InitiateLoginReq -> FlowHandler InitiateLoginRes
initiateLogin loginRes = undefined

login :: Text -> LoginReq -> FlowHandler Value
login tokenId req = do
  case req ^. Accessor.action of
    VERIFY -> undefined
    RESEND -> undefined
-------------------------------

---- Pass Application Flow ------
--
createPassApplication ::
  Maybe Text
  -> CreatePassApplicationReq
  -> FlowHandler PassApplicationRes
createPassApplication regToken req = undefined

listPassApplication ::
  Maybe Text
  -> ListPassApplicationReq
  -> FlowHandler ListPassApplicationRes
listPassApplication regToken req = undefined

getPassApplicationById ::
  Maybe Text
  -> Text
  -> FlowHandler PassApplicationRes
getPassApplicationById regToken applicationId = undefined
