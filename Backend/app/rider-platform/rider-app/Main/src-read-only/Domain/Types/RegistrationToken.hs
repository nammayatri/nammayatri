{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RegistrationToken where

import Data.Aeson
import qualified Domain.Types.PartnerOrganization
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data RegistrationToken = RegistrationToken
  { id :: Kernel.Types.Id.Id Domain.Types.RegistrationToken.RegistrationToken,
    token :: Kernel.Prelude.Text,
    attempts :: Kernel.Prelude.Int,
    authMedium :: Domain.Types.RegistrationToken.Medium,
    authType :: Domain.Types.RegistrationToken.LoginType,
    authValueHash :: Kernel.Prelude.Text,
    verified :: Kernel.Prelude.Bool,
    authExpiry :: Kernel.Prelude.Int,
    tokenExpiry :: Kernel.Prelude.Int,
    entityId :: Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Text,
    entityType :: Domain.Types.RegistrationToken.RTEntityType,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime,
    info :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdViaPartnerOrgId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PartnerOrganization.PartnerOrganization)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data LoginType = OTP | PASSWORD | DIRECT | OAUTH deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data Medium = SMS | WHATSAPP | EMAIL | SIGNATURE | PARTNER_ORG deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data RTEntityType = CUSTOMER | USER deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''LoginType)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''Medium)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''RTEntityType)
