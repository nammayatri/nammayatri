{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.CommunicationEngine.Domain.Types.CommunicationDelivery where

import Data.Aeson
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Lib.CommunicationEngine.Domain.Types.Communication
import qualified Tools.Beam.UtilsTH

data CommunicationDelivery = CommunicationDelivery
  { channel :: Lib.CommunicationEngine.Domain.Types.Communication.ChannelType,
    communicationId :: Kernel.Types.Id.Id Lib.CommunicationEngine.Domain.Types.Communication.Communication,
    createdAt :: Kernel.Prelude.UTCTime,
    deliveredAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    failureReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fleetOwnerId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Lib.CommunicationEngine.Domain.Types.CommunicationDelivery.CommunicationDelivery,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    operatorId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    readAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    recipientId :: Kernel.Prelude.Text,
    recipientRole :: Lib.CommunicationEngine.Domain.Types.CommunicationDelivery.CommunicationRecipientRole,
    status :: Lib.CommunicationEngine.Domain.Types.CommunicationDelivery.DeliveryStatus,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data CommunicationRecipientRole = RR_DRIVER | RR_FLEET_OWNER | RR_OPERATOR | RR_ADMIN deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data DeliveryStatus = DS_PENDING | DS_SENT | DS_DELIVERED | DS_READ | DS_FAILED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''CommunicationRecipientRole))

$(mkHttpInstancesForEnum (''CommunicationRecipientRole))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''DeliveryStatus))

$(mkHttpInstancesForEnum (''DeliveryStatus))
