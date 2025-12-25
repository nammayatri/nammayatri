{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MessageReport (module Domain.Types.MessageReport, module ReExport) where

import Data.Aeson
import Domain.Types.Extra.MessageReport as ReExport
import qualified Domain.Types.Extra.MessageReport
import qualified Domain.Types.Message
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data MessageReport = MessageReport
  { createdAt :: Kernel.Prelude.UTCTime,
    deliveryStatus :: Domain.Types.MessageReport.DeliveryStatus,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Driver,
    likeStatus :: Kernel.Prelude.Bool,
    messageDynamicFields :: Domain.Types.Extra.MessageReport.MessageDynamicFieldsType,
    messageId :: Kernel.Types.Id.Id Domain.Types.Message.Message,
    readStatus :: Kernel.Prelude.Bool,
    reply :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data DeliveryStatus = Success | Failed | Queued | Sending deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''DeliveryStatus)
