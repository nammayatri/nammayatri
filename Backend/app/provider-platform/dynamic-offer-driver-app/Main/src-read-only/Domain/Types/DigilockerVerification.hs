{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DigilockerVerification where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DigilockerVerification = DigilockerVerification
  { accessToken :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    accessTokenExpiresAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    authorizationCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    codeChallenge :: Kernel.Prelude.Text,
    codeMethod :: Kernel.Prelude.Text,
    codeVerifier :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    docStatus :: Data.Aeson.Value,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Kernel.Types.Id.Id Domain.Types.DigilockerVerification.DigilockerVerification,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    responseCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    responseDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    scope :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sessionStatus :: Domain.Types.DigilockerVerification.SessionStatus,
    stateId :: Kernel.Prelude.Text,
    tokenResponse :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data SessionStatus = PENDING | SUCCESS | FAILED | CONSENT_DENIED deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''SessionStatus)
