{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DigilockerVerification where

import Data.Aeson
import qualified Domain.Types.DocStatus
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.VehicleCategory
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DigilockerVerificationE e = DigilockerVerification
  { accessToken :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    accessTokenExpiresAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    authorizationCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    codeChallenge :: Kernel.Prelude.Text,
    codeMethod :: Kernel.Prelude.Text,
    codeVerifier :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    docStatus :: Domain.Types.DocStatus.DocStatusMap,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Kernel.Types.Id.Id Domain.Types.DigilockerVerification.DigilockerVerification,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    responseCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    responseDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    scope :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sessionStatus :: Domain.Types.DigilockerVerification.SessionStatus,
    stateId :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleCategory :: Domain.Types.VehicleCategory.VehicleCategory
  }
  deriving (Generic)

type DigilockerVerification = DigilockerVerificationE 'AsEncrypted

type DecryptedDigilockerVerification = DigilockerVerificationE 'AsUnencrypted

instance EncryptedItem DigilockerVerification where
  type Unencrypted DigilockerVerification = (DecryptedDigilockerVerification, HashSalt)
  encryptItem (entity, salt) = do
    accessToken_ <- encryptItem $ (,salt) <$> accessToken entity
    pure
      DigilockerVerification
        { accessToken = accessToken_,
          accessTokenExpiresAt = accessTokenExpiresAt entity,
          authorizationCode = authorizationCode entity,
          codeChallenge = codeChallenge entity,
          codeMethod = codeMethod entity,
          codeVerifier = codeVerifier entity,
          createdAt = createdAt entity,
          docStatus = docStatus entity,
          driverId = driverId entity,
          id = id entity,
          merchantId = merchantId entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          responseCode = responseCode entity,
          responseDescription = responseDescription entity,
          scope = scope entity,
          sessionStatus = sessionStatus entity,
          stateId = stateId entity,
          updatedAt = updatedAt entity,
          vehicleCategory = vehicleCategory entity
        }
  decryptItem entity = do
    accessToken_ <- fmap fst <$> decryptItem (accessToken entity)
    pure
      ( DigilockerVerification
          { accessToken = accessToken_,
            accessTokenExpiresAt = accessTokenExpiresAt entity,
            authorizationCode = authorizationCode entity,
            codeChallenge = codeChallenge entity,
            codeMethod = codeMethod entity,
            codeVerifier = codeVerifier entity,
            createdAt = createdAt entity,
            docStatus = docStatus entity,
            driverId = driverId entity,
            id = id entity,
            merchantId = merchantId entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            responseCode = responseCode entity,
            responseDescription = responseDescription entity,
            scope = scope entity,
            sessionStatus = sessionStatus entity,
            stateId = stateId entity,
            updatedAt = updatedAt entity,
            vehicleCategory = vehicleCategory entity
          },
        ""
      )

instance EncryptedItem' DigilockerVerification where
  type UnencryptedItem DigilockerVerification = DecryptedDigilockerVerification
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst

data SessionStatus = PENDING | SUCCESS | FAILED | CONSENT_DENIED deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema), (ToParamSchema))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''SessionStatus))
