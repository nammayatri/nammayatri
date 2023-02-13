module Domain.Types.Message.MessageReport where

import Data.OpenApi
import Domain.Types.Person (Driver)
import qualified Domain.Types.Message.Message as Msg
import qualified Data.Map as M
import Kernel.Types.Id
import Kernel.Prelude

type MessageDynamicFieldsType = M.Map Text Text

data DeliveryStatus = Success | Failed | Pending deriving (Generic, ToSchema, Show, Read, ToJSON, FromJSON)

data MessageReport = MessageReport 
  { messageId :: Id Msg.Message,
    driverId :: Id Driver,
    deliveryStatus :: DeliveryStatus, 
    readStatus :: Bool,
    reply :: Maybe Text, 
    messageDynamicFields :: MessageDynamicFieldsType,
    createdAt :: UTCTime, 
    updatedAt :: UTCTime
  }
