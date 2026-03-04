{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Communication (module Domain.Types.Communication, module ReExport) where

import Data.Aeson
import Domain.Types.Extra.Communication as ReExport
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data Communication = Communication
  { body :: Kernel.Prelude.Text,
    channels :: [Domain.Types.Communication.ChannelType],
    contentType :: Domain.Types.Communication.CommunicationContentType,
    createdAt :: Kernel.Prelude.UTCTime,
    ctaButton :: Kernel.Prelude.Maybe Domain.Types.Communication.CTAButton,
    domain :: Domain.Types.Communication.CommunicationDomain,
    htmlBody :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.Communication.Communication,
    mediaUrls :: Kernel.Prelude.Maybe Data.Aeson.Value,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    scheduledAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    senderDisplayName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    senderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    senderRole :: Domain.Types.Communication.CommunicationSenderRole,
    status :: Domain.Types.Communication.CommunicationStatus,
    templateId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    templateName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    title :: Kernel.Prelude.Text,
    triggerType :: Domain.Types.Communication.CommunicationTriggerType,
    updatedAt :: Kernel.Prelude.UTCTime,
    variables :: Kernel.Prelude.Maybe Data.Aeson.Value
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data CTAButton = CTAButton {label :: Kernel.Prelude.Text, linkType :: Kernel.Prelude.Text, url :: Kernel.Prelude.Text} deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data ChannelType = CH_PUSH | CH_SMS | CH_EMAIL | CH_WHATSAPP | CH_WEB deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data CommunicationContentType = CT_TEXT | CT_IMAGE | CT_VIDEO deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data CommunicationDomain = CD_FLEET | CD_RIDE_HAILING | CD_GENERAL deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data CommunicationSenderRole = SR_ADMIN | SR_OPERATOR | SR_FLEET_OWNER deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data CommunicationStatus = ST_DRAFT | ST_QUEUED | ST_SENDING | ST_SENT | ST_FAILED | ST_SCHEDULED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data CommunicationTriggerType = TT_MANUAL | TT_SYSTEM | TT_SCHEDULED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ChannelType)

$(mkHttpInstancesForEnum ''ChannelType)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''CommunicationContentType)

$(mkHttpInstancesForEnum ''CommunicationContentType)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''CommunicationDomain)

$(mkHttpInstancesForEnum ''CommunicationDomain)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''CommunicationSenderRole)

$(mkHttpInstancesForEnum ''CommunicationSenderRole)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''CommunicationStatus)

$(mkHttpInstancesForEnum ''CommunicationStatus)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''CommunicationTriggerType)

$(mkHttpInstancesForEnum ''CommunicationTriggerType)
