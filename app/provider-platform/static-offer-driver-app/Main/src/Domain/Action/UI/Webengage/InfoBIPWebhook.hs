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
    EsqDBFlow m r,
    CoreMetrics m,
    Log m
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
