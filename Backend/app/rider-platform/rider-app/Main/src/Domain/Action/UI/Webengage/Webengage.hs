{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Webengage.Webengage where

import qualified Domain.Types.Person as Person hiding (PersonAPIEntity (id), PersonE (id))
import Domain.Types.Webengage
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Infobip.Flow as IF
import qualified Kernel.External.Infobip.Types as EIF hiding (id)
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Utils.Common
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
    CoreMetrics m
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
  _ <- QWeb.create webengage
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
