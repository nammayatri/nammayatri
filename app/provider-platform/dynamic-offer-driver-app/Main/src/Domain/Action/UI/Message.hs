module Domain.Action.UI.Message where

import Data.OpenApi hiding (info, url, title, description)
import qualified Domain.Types.Message.Message as Domain
import qualified Domain.Types.Person as SP
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Message.MessageReport as MRQ 
import qualified Storage.Queries.Person as QP
import Tools.Error
import Environment
import Kernel.External.Types (Language(ENGLISH))
import qualified Domain.Types.Message.MessageReport as Domain
import qualified Domain.Types.Message.MediaFile as MF 


data MessageAPIEntityResponse = MessageAPIEntityResponse
  { title :: Text, 
    description :: Text,
    _type :: Domain.MessageType, 
    status :: Domain.DeliveryStatus, 
    readStatus :: Bool, 
    messageId :: Id Domain.Message,
    mediaFiles :: [Id MF.MediaFile]
  } deriving (Generic, ToSchema, ToJSON, FromJSON)

newtype MessageReplyReq = MessageReplyReq { reply :: Text }
    deriving (Generic, ToSchema, ToJSON, FromJSON)

messageList :: Id SP.Person -> Maybe Int -> Maybe Int -> Flow [MessageAPIEntityResponse]
messageList driverId mbLimit mbOffset = do
    person <- Esq.runInReplica (QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId))
    messageDetails <- Esq.runInReplica $ MRQ.findByDriverIdAndLanguage (cast driverId) (fromMaybe ENGLISH person.language) mbLimit mbOffset

    return $ makeMessageAPIEntity <$> messageDetails
    where 
        makeMessageAPIEntity (messageReport, rawMessage, messageTranslation) = do 
          MessageAPIEntityResponse 
            { title = maybe rawMessage.title (.title) messageTranslation, 
              description = maybe rawMessage.description (.description) messageTranslation,
              _type = rawMessage._type,
              status = messageReport.deliveryStatus,
              readStatus = messageReport.readStatus,
              messageId = rawMessage.id,
              mediaFiles = rawMessage.mediaFiles
            }

messageSeen :: Id SP.Person -> Id Domain.Message -> Flow APISuccess
messageSeen driverId messageId = do
    _ <- Esq.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
    Esq.runTransaction $ MRQ.updateSeenAndReplyByMessageIdAndDriverId messageId (cast driverId) True Nothing 
    return Success

messageResponse :: Id SP.Person -> Id Domain.Message -> MessageReplyReq -> Flow APISuccess
messageResponse driverId messageId MessageReplyReq {..} = do
    _ <- Esq.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
    Esq.runTransaction $ MRQ.updateSeenAndReplyByMessageIdAndDriverId messageId (cast driverId) True (Just reply) 
    return Success
