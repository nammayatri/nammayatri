module Domain.Action.UI.Webengage.InfoBIPWebhook where

import qualified Beckn.External.Infobip.Flow as IF
import qualified Beckn.External.Infobip.Types as IT hiding (id)
import Beckn.Prelude hiding (error)
import Beckn.Storage.Esqueleto hiding (from)
import Beckn.Types.APISuccess
import Beckn.Utils.Common
import qualified Storage.Queries.Webengage as QW
import Tools.Error
import Tools.Metrics

newtype StatusRes = StatusRes
  { results :: [Message]
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, ToSchema)

data Message = Message
  { to :: Text,
    status :: SMSStatus,
    messageId :: Text,
    smsCount :: Integer
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, ToSchema)

data SMSStatus = SMSStatus
  { description :: Text,
    groupId :: Int,
    groupName :: Text,
    id :: Int,
    name :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, ToSchema)

sendStatus ::
  ( HasFlowEnv m r '["webengageCfg" ::: IT.WebengageConfig],
    EncFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    Log m
  ) =>
  StatusRes ->
  m APISuccess
sendStatus req = do
  message <- fromMaybeM (InvalidRequest "Response Not Found") $ listToMaybe req.results
  webengageCfg <- asks (.webengageCfg)
  webengageData <- QW.findByInfoMsgId message.messageId >>= fromMaybeM (PersonDoesNotExist message.messageId)
  let version = webengageData.version
  let webMsgId = webengageData.webMessageId
  let toNumber = webengageData.toNumber
  let infoBipStatus = message.status.groupName
  runTransaction $ QW.updateStatus message.messageId infoBipStatus
  webengageRes <- buildwebengageRes version webMsgId toNumber infoBipStatus
  _ <- IF.callWebengageWebhook webengageCfg webengageRes
  return Success
  where
    buildwebengageRes version webMsgId toNumber infoBipStatus = do
      if infoBipStatus == "DELIVERED"
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