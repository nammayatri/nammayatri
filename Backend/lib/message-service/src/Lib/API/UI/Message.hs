{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This prograFlow is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.API.UI.Message where

import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Domain.Action.UI.Message as DMessage
import qualified Lib.Domain.Types.Message.Message as Message
import Servant

type CommonAPI =
  "message"
    :> ( "list"
           :> QueryParam "limit" Int
           :> QueryParam "offset" Int
           :> Get '[JSON] [DMessage.MessageAPIEntityResponse]
           :<|> Capture "messageId" (Id Message.Message)
             :> "seen"
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

-- type API =
--   "message"
--     :> ( "list"
--            :> TokenAuth
--            :> QueryParam "limit" Int
--            :> QueryParam "offset" Int
--            :> Get '[JSON] [DMessage.MessageAPIEntityResponse]
--            :<|> Capture "messageId" (Id Message.Message)
--              :> "seen"
--              :> TokenAuth
--              :> Put '[JSON] APISuccess
--            :<|> Capture "messageId" (Id Message.Message)
--              :> "like"
--              :> TokenAuth
--              :> Put '[JSON] APISuccess
--            :<|> Capture "messageId" (Id Message.Message)
--              :> "response"
--              :> TokenAuth
--              :> ReqBody '[JSON] DMessage.MessageReplyReq
--              :> Put '[JSON] APISuccess
--            :<|> "media" -- TODO : need to remove this apis once S3 is done.
--              :> MandatoryQueryParam "filePath" Text
--              :> TokenAuth
--              :> Get '[JSON] Text
--            :<|> Capture "messageId" (Id Message.Message)
--              :> TokenAuth
--              :> Get '[JSON] DMessage.MessageAPIEntityResponse
--        )
