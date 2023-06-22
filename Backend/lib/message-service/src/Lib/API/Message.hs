{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.API.Message where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Message as Common
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Domain.Action.UI.Message as DMessage
import qualified Lib.Domain.Types.Message.Message as Message
import Servant

type UiAPI =
  "message"
    :> ( "list"
           :> QueryParam "limit" Int
           :> QueryParam "offset" Int
           :> Get '[JSON] [DMessage.MessageAPIEntityResponse]
           :<|> Capture "messageId" (Id Message.Message)
             :> "seen"
             :> Put '[JSON] APISuccess
           :<|> Capture "messageId" (Id Message.Message)
             :> "like"
             :> Put '[JSON] APISuccess
           :<|> Capture "messageId" (Id Message.Message)
             :> "response"
             :> ReqBody '[JSON] DMessage.MessageReplyReq
             :> Put '[JSON] APISuccess
           :<|> "media" -- TODO : need to remove this apis once S3 is done.
             :> MandatoryQueryParam "filePath" Text
             :> Get '[JSON] Text
           :<|> Capture "messageId" (Id Message.Message)
             :> Get '[JSON] DMessage.MessageAPIEntityResponse
       )

type DashboardAPI =
  "message"
    :> ( Common.UploadFileAPI
           :<|> Common.AddLinkAPI
           :<|> Common.AddMessageAPI
           :<|> Common.SendMessageAPI
           :<|> Common.MessageListAPI
           :<|> Common.MessageInfoAPI
           :<|> Common.MessageDeliveryInfoAPI
           :<|> Common.MessageReceiverListAPI
       )
