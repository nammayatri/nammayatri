{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.OperationHubRequests where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.OperationHub
import qualified Domain.Types.Person
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data OperationHubRequestsE e = OperationHubRequests
  { driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    fulfilledAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.OperationHubRequests.OperationHubRequests,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    operationHubId :: Kernel.Types.Id.Id Domain.Types.OperationHub.OperationHub,
    operatorId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    registrationNo :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    remarks :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    requestStatus :: Domain.Types.OperationHubRequests.RequestStatus,
    requestType :: Domain.Types.OperationHubRequests.RequestType,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type OperationHubRequests = OperationHubRequestsE 'AsEncrypted

type DecryptedOperationHubRequests = OperationHubRequestsE 'AsUnencrypted

instance EncryptedItem OperationHubRequests where
  type Unencrypted OperationHubRequests = (DecryptedOperationHubRequests, HashSalt)
  encryptItem (entity, salt) = do
    registrationNo_ <- encryptItem (registrationNo entity, salt)
    pure
      OperationHubRequests
        { driverId = driverId entity,
          fulfilledAt = fulfilledAt entity,
          id = id entity,
          merchantId = merchantId entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          operationHubId = operationHubId entity,
          operatorId = operatorId entity,
          registrationNo = registrationNo_,
          remarks = remarks entity,
          requestStatus = requestStatus entity,
          requestType = requestType entity,
          createdAt = createdAt entity,
          updatedAt = updatedAt entity
        }
  decryptItem entity = do
    registrationNo_ <- fst <$> decryptItem (registrationNo entity)
    pure
      ( OperationHubRequests
          { driverId = driverId entity,
            fulfilledAt = fulfilledAt entity,
            id = id entity,
            merchantId = merchantId entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            operationHubId = operationHubId entity,
            operatorId = operatorId entity,
            registrationNo = registrationNo_,
            remarks = remarks entity,
            requestStatus = requestStatus entity,
            requestType = requestType entity,
            createdAt = createdAt entity,
            updatedAt = updatedAt entity
          },
        ""
      )

instance EncryptedItem' OperationHubRequests where
  type UnencryptedItem OperationHubRequests = DecryptedOperationHubRequests
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst

data RequestStatus = PENDING | APPROVED | REJECTED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data RequestType = ONBOARDING_INSPECTION | REGULAR_INSPECTION deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''RequestStatus)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''RequestType)
