{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Webengage.InfoBIPWebhook where

import qualified Kernel.External.Infobip.Flow as IF
import qualified Kernel.External.Infobip.Types as IT hiding (id)
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Utils.Common
import Kernel.Utils.Dhall (FromDhall)
import qualified Storage.Queries.Webengage as QW
import Tools.Error
import Tools.Metrics

newtype StatusRes = StatusRes
  { messages :: [Message]
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, FromDhall, ToSchema)

data Message = Message
  { to :: Text,
    status :: SMSStatus,
    messageId :: Text,
    smsCount :: Integer
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, FromDhall, ToSchema)

data SMSStatus = SMSStatus
  { description :: Text,
    groupId :: Int,
    groupName :: Text,
    id :: Int,
    name :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, FromDhall, ToSchema)

sendStatus ::
  ( HasFlowEnv m r '["webengageCfg" ::: IT.WebengageConfig],
    EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m
  ) =>
  StatusRes ->
  m APISuccess
sendStatus req = withTransactionIdLogTag' (head req.messages).messageId $ do
  webengageCfg <- asks (.webengageCfg)
  let infoBipRes = head req.messages
  webengageData <- QW.findByInfoMsgId infoBipRes.messageId >>= fromMaybeM (PersonDoesNotExist infoBipRes.messageId)
  let version = webengageData.version
  let webMsgId = webengageData.webMessageId
  let toNumber = webengageData.toNumber
  let infoBipStatus = infoBipRes.status.groupName
  webengageRes <- buildwebengageRes version webMsgId toNumber infoBipStatus
  _ <- IF.callWebengageWebhook webengageCfg webengageRes
  return Success
  where
    buildwebengageRes version webMsgId toNumber infoBipStatus = do
      if infoBipStatus == "DELIVERED_TO_HANDSET"
        then
          return $
            IT.WebengageRes
              { version = version,
                messageId = webMsgId,
                toNumber = toNumber,
                status = "sms_sent",
                statusCode = "0"
              }
        else
          return $
            IT.WebengageRes
              { version = version,
                messageId = webMsgId,
                toNumber = toNumber,
                status = "sms_failed",
                statusCode = "2009"
              }
