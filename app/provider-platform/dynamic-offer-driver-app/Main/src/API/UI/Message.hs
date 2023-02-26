module API.UI.Message where

import qualified Domain.Action.UI.Message as DMessage
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
    :<|> messageResponse
    :<|> fetchMedia
    :<|> getMessage

messageList :: Id SP.Person -> Maybe Int -> Maybe Int -> FlowHandler [DMessage.MessageAPIEntityResponse]
messageList driverId mbLimit = withFlowHandlerAPI . DMessage.messageList driverId mbLimit

getMessage :: Id Message.Message -> Id SP.Person -> FlowHandler DMessage.MessageAPIEntityResponse
getMessage msgId driverId = withFlowHandlerAPI $ DMessage.getMessage driverId msgId

messageSeen :: Id Message.Message -> Id SP.Person -> FlowHandler APISuccess
messageSeen msgId driverId = withFlowHandlerAPI $ DMessage.messageSeen driverId msgId

messageResponse :: Id Message.Message -> Id SP.Person -> DMessage.MessageReplyReq -> FlowHandler APISuccess
messageResponse msgId driverId = withFlowHandlerAPI . DMessage.messageResponse driverId msgId

fetchMedia :: Text -> Id SP.Person -> FlowHandler Text
fetchMedia filePath driverId = withFlowHandlerAPI $ DMessage.fetchMedia driverId filePath
