module Domain.Action.UI.Webengage.Webengage where

import Beckn.External.Encryption (decrypt)
import qualified Beckn.External.Infobip.Flow as IF
import qualified Beckn.External.Infobip.Types as EIF hiding (id)
import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Person as Person hiding (id)
import Domain.Types.Webengage
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Webengage as QWeb
import Tools.Error
import Tools.Metrics

data WebengageReq = WebengageReq
  { version :: Text,
    smsData :: SmsData,
    metadata :: MetaData
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data SmsData = SmsData
  { toNumber :: Id Person.Person,
    fromNumber :: Text,
    body :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data MetaData = MetaData
  { campaignType :: Text,
    custom :: Custom,
    timestamp :: UTCTime, --to be confirmed
    messageId :: Text,
    indiaDLT :: IndiaDLT
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

newtype Custom = Custom
  { api_key :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data IndiaDLT = IndiaDLT
  { principalEntityId :: Text,
    contentTemplateId :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

newtype WebengageRes = WebengageRes
  { status :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

callInfobip ::
  ( HasFlowEnv m r '["infoBIPCfg" ::: EIF.InfoBIPConfig],
    EncFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    Log m
  ) =>
  WebengageReq ->
  m WebengageRes
callInfobip req = withTransactionIdLogTag' req.metadata.messageId $ do
  infoBIPCfg <- asks (.infoBIPCfg)
  unless (req.metadata.custom.api_key == infoBIPCfg.token) $ throwError $ InvalidRequest "Invalid Authorization Token"
  let personId = req.smsData.toNumber
  let smsBody = req.smsData.body
  let entityId = req.metadata.indiaDLT.principalEntityId
  let templetId = req.metadata.indiaDLT.contentTemplateId
  let version = req.version
  let webMsgId = req.metadata.messageId
  person <- Person.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  decPerson <- decrypt person
  mobileNumber <- fromMaybeM (PersonDoesNotExist personId.getId) decPerson.mobileNumber
  countryCode <- fromMaybeM (PersonDoesNotExist personId.getId) decPerson.mobileCountryCode
  infoBipRes <- IF.sendSms infoBIPCfg smsBody (countryCode <> mobileNumber) entityId templetId
  let infoMessage = head infoBipRes.messages
  let status = Just infoMessage.status.groupName
  webengage <- buildWebengage version entityId templetId infoMessage.messageId webMsgId personId.getId status
  runTransaction $ QWeb.create webengage
  return $
    WebengageRes
      { status = "sms_accepted"
      }
  where
    buildWebengage version entityId templetId infoMsgId webMsgId personId status = do
      id <- generateGUID
      return $
        Webengage
          { id = id,
            version = version,
            contentTemplateId = templetId,
            principalEntityId = entityId,
            infoMessageId = infoMsgId,
            webMessageId = webMsgId,
            toNumber = personId,
            status = status
          }
