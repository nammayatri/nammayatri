{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Communication.Storage.Beam.CommunicationDelivery where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.Communication.Domain.Types.Communication
import qualified Lib.Communication.Domain.Types.CommunicationDelivery

data CommunicationDeliveryT f = CommunicationDeliveryT
  { channel :: B.C f Lib.Communication.Domain.Types.Communication.ChannelType,
    communicationId :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    deliveredAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    failureReason :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fleetOwnerId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    operatorId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    readAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    recipientId :: B.C f Kernel.Prelude.Text,
    recipientRole :: B.C f Lib.Communication.Domain.Types.CommunicationDelivery.CommunicationRecipientRole,
    status :: B.C f Lib.Communication.Domain.Types.CommunicationDelivery.DeliveryStatus,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table CommunicationDeliveryT where
  data PrimaryKey CommunicationDeliveryT f = CommunicationDeliveryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = CommunicationDeliveryId . id

type CommunicationDelivery = CommunicationDeliveryT Identity

$(enableKVPG ''CommunicationDeliveryT ['id] [['communicationId]])

$(mkTableInstancesGenericSchema ''CommunicationDeliveryT "communication_delivery")
