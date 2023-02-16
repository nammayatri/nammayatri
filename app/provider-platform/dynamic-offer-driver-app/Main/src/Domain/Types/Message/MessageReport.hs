module Domain.Types.Message.MessageReport where

import qualified Data.Map as M
import Data.OpenApi
import qualified Domain.Types.Message.Message as Msg
import Domain.Types.Person (Driver)
import Kernel.Prelude
import Kernel.Types.Id

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
