module Domain.Action.UI.Webengage where

import Beckn.External.Encryption (decrypt)
import qualified Beckn.External.Infobip.Flow as IF
import Beckn.External.Infobip.Types hiding (id)
import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Person as Person hiding (id)
import qualified Storage.Queries.Person as Person
import Tools.Error
import Tools.Metrics

data WebengageReq = WebengageReq
  { version :: Text, --to be confirmed
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
    indiaDLT :: Maybe IndiaDLT
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

newtype Custom = Custom
  { api_key :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

newtype IndiaDLT = IndiaDLT
  { principalEntityId :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data WebengageRes = WebengageRes
  { version :: Text, --to be confirmed
    messageId :: Text,
    toNumber :: Id Person.Person,
    status :: Text, -- to be confirmed
    statusCode :: Text -- to be confirmed
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

callInfobip ::
  ( HasFlowEnv m r '["infoBIPCfg" ::: InfoBIPConfig],
    EncFlow m r,
    EsqDBReplicaFlow m r,
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
  person <- Person.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  decPerson <- decrypt person
  mobileNumber <- fromMaybeM (PersonDoesNotExist personId.getId) decPerson.mobileNumber
  countryCode <- fromMaybeM (PersonDoesNotExist personId.getId) decPerson.mobileCountryCode
  infobipRes <- IF.sendSms infoBIPCfg smsBody (countryCode <> mobileNumber)
  buildWebengageRes infobipRes req

buildWebengageRes :: EncFlow m r => SMSRes -> WebengageReq -> m WebengageRes
buildWebengageRes infobipres req = do
  if isNothing infobipres.requestError
    then
      return $
        WebengageRes
          { version = req.version,
            messageId = req.metadata.messageId,
            toNumber = req.smsData.toNumber,
            status = "sms_failed",
            statusCode = "2007"
          }
    else
      return $
        WebengageRes
          { version = req.version,
            messageId = req.metadata.messageId,
            toNumber = req.smsData.toNumber,
            status = "sms_sent",
            statusCode = "0"
          }
