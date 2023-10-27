{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Message where

import qualified Domain.Action.UI.Message as DMessage
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Message.Message as Message
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API =
  "message"
    :> ( "list"
           :> TokenAuth
           :> QueryParam "limit" Int
           :> QueryParam "offset" Int
           :> Get '[JSON] [DMessage.MessageAPIEntityResponse]
           :<|> Capture "messageId" (Id Message.Message)
             :> "seen"
             :> TokenAuth
             :> Put '[JSON] APISuccess
           :<|> Capture "messageId" (Id Message.Message)
             :> "like"
             :> TokenAuth
             :> Put '[JSON] APISuccess
           :<|> Capture "messageId" (Id Message.Message)
             :> "response"
             :> TokenAuth
             :> ReqBody '[JSON] DMessage.MessageReplyReq
             :> Put '[JSON] APISuccess
           :<|> "media" -- TODO : need to remove this apis once S3 is done.
             :> MandatoryQueryParam "filePath" Text
             :> TokenAuth
             :> Get '[JSON] Text
           :<|> Capture "messageId" (Id Message.Message)
             :> TokenAuth
             :> Get '[JSON] DMessage.MessageAPIEntityResponse
       )

handler :: FlowServer API
handler =
  messageList
    :<|> messageSeen
    :<|> messageLiked
    :<|> messageResponse
    :<|> fetchMedia
    :<|> getMessage

messageList :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Int -> Maybe Int -> FlowHandler [DMessage.MessageAPIEntityResponse]
messageList (driverId, merchantId, merchantOpCityId) mbLimit = withFlowHandlerAPI . DMessage.messageList (driverId, merchantId, merchantOpCityId) mbLimit

getMessage :: Id Message.Message -> (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler DMessage.MessageAPIEntityResponse
getMessage msgId (driverId, merchantId, merchantOpCityId) = withFlowHandlerAPI $ DMessage.getMessage (driverId, merchantId, merchantOpCityId) msgId

messageSeen :: Id Message.Message -> (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler APISuccess
messageSeen msgId (driverId, merchantId, merchantOpCityId) = withFlowHandlerAPI $ DMessage.messageSeen (driverId, merchantId, merchantOpCityId) msgId

messageLiked :: Id Message.Message -> (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler APISuccess
messageLiked msgId (driverId, merchantId, merchantOpCityId) = withFlowHandlerAPI $ DMessage.messageLiked (driverId, merchantId, merchantOpCityId) msgId

messageResponse :: Id Message.Message -> (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> DMessage.MessageReplyReq -> FlowHandler APISuccess
messageResponse msgId (driverId, merchantId, merchantOpCityId) = withFlowHandlerAPI . DMessage.messageResponse (driverId, merchantId, merchantOpCityId) msgId

fetchMedia :: Text -> (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler Text
fetchMedia filePath (driverId, merchantId, merchantOpCityId) = withFlowHandlerAPI $ DMessage.fetchMedia (driverId, merchantId, merchantOpCityId) filePath
