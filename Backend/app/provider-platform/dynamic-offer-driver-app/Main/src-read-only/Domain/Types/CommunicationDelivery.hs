{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.CommunicationDelivery where

import Data.Aeson
import qualified Domain.Types.Communication
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data CommunicationDelivery = CommunicationDelivery
  { channel :: Domain.Types.Communication.ChannelType,
    communicationId :: Kernel.Types.Id.Id Domain.Types.Communication.Communication,
    createdAt :: Kernel.Prelude.UTCTime,
    deliveredAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    failureReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fleetOwnerId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.CommunicationDelivery.CommunicationDelivery,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    operatorId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    readAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    recipientId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    recipientRole :: Domain.Types.CommunicationDelivery.CommunicationRecipientRole,
    status :: Domain.Types.CommunicationDelivery.DeliveryStatus,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data CommunicationRecipientRole = RR_DRIVER | RR_FLEET_OWNER | RR_OPERATOR | RR_ADMIN deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data DeliveryStatus = DS_PENDING | DS_SENT | DS_DELIVERED | DS_READ | DS_FAILED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''CommunicationRecipientRole)

$(mkHttpInstancesForEnum ''CommunicationRecipientRole)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''DeliveryStatus)

$(mkHttpInstancesForEnum ''DeliveryStatus)
