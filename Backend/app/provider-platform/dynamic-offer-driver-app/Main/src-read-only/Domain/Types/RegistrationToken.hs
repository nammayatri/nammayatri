{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RegistrationToken where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data RegistrationToken = RegistrationToken
  { alternateNumberAttempts :: Kernel.Prelude.Int,
    attempts :: Kernel.Prelude.Int,
    authExpiry :: Kernel.Prelude.Int,
    authMedium :: Domain.Types.RegistrationToken.Medium,
    authType :: Domain.Types.RegistrationToken.LoginType,
    authValueHash :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    entityId :: Kernel.Prelude.Text,
    entityType :: Domain.Types.RegistrationToken.RTEntityType,
    id :: Kernel.Types.Id.Id Domain.Types.RegistrationToken.RegistrationToken,
    info :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    token :: Kernel.Prelude.Text,
    tokenExpiry :: Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime,
    verified :: Kernel.Prelude.Bool
  }
  deriving (Generic, Show, Eq)

data LoginType = OTP | PASSWORD | OAUTH | DIRECT deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data Medium = SMS | EMAIL | SIGNATURE deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data RTEntityType = CUSTOMER | USER deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''LoginType)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''Medium)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''RTEntityType)
