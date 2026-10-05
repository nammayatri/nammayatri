{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.EmailDelivery where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data EmailDelivery = EmailDelivery
  { bounceSubType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    bounceType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    deliveredAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    failureReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.EmailDelivery.EmailDelivery,
    lastEventAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    ownerId :: Kernel.Prelude.Text,
    ownerType :: Domain.Types.EmailDelivery.EmailDeliveryOwnerType,
    provider :: Kernel.Prelude.Maybe Domain.Types.EmailDelivery.EmailDeliveryProvider,
    providerMessageId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sentAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    status :: Domain.Types.EmailDelivery.EmailDeliveryStatus,
    toAddress :: Kernel.Prelude.Text,
    triggeredBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data EmailDeliveryOwnerType = TDS_RECORD | COMM_DELIVERY deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema))

data EmailDeliveryProvider = SES | SENDGRID deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema))

data EmailDeliveryStatus = SENDING | SENT | DELAYED | DELIVERED | BOUNCED | REJECTED | COMPLAINED | FAILED deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''EmailDeliveryOwnerType))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''EmailDeliveryProvider))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''EmailDeliveryStatus))
