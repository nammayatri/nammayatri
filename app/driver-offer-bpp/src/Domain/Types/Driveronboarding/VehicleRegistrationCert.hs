{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.Driveronboarding.VehicleRegistrationCert where

import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.JSON (constructorsToLowerOptions)
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.Aeson
  ( Options (constructorTagModifier),
    defaultOptions,
  )
import Data.OpenApi
import Domain.Types.Person (Person)

data VerificationStatus = PENDING | VALID | INVALID
  deriving (Show, Eq, Read, Generic, Enum, Bounded, FromJSON, ToJSON, ToSchema)

-- added valid and invalid inorder to accomodate validation results
data IdfyStatus = IN_PROGRESS | FAILED | COMPLETED
  deriving (Show, Eq, Read, Generic, Enum, Bounded)

instance ToSchema IdfyStatus where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance FromJSON IdfyStatus where
  parseJSON = genericParseJSON constructorsToLowerOptions

instance ToJSON IdfyStatus where
  toJSON = genericToJSON constructorsToLowerOptions

data COV = W_NT | W_T | W_CAB | HGV_T | HMV_HGV | HMV | HTV | LMV | LMV_NT | LMV_T | LMV_CAB | LMV_HMV | LTV | MCWG | MCWOG | HPMV | MGV | MMV | LDRXCV | PSV_BUS | TRANS | TRCTOR | Others
  deriving (Show, Eq, Read, Generic, Enum, Bounded, Ord, ToSchema)

instance FromJSON COV where
  parseJSON = genericParseJSON constructorForCOVFromJson

instance ToJSON COV where
  toJSON = genericToJSON constructorForCOVToJson

constructorForCOVFromJson :: Options
constructorForCOVFromJson =
  defaultOptions
    { constructorTagModifier = \case
        "3W_NT" -> "W_NT"
        "3W_T" -> "W_T"
        "3W_CAB" -> "W_CAB"
        val -> val
    }

constructorForCOVToJson :: Options
constructorForCOVToJson =
  defaultOptions
    { constructorTagModifier = \case
        "W_NT" -> "3W_NT"
        "W_T" -> "3W_T"
        "W_CAB" -> "3W_CAB"
        val -> val
    }

-- here we should only check vehicle class with three wheeler vehicle type only

data VehicleRegistrationCertE e = VehicleRegistrationCert
  { id :: Id VehicleRegistrationCert,
    driverId :: Id Person,
    vehicleRegistrationCertNumber :: Maybe (EncryptedHashedField e Text), -- remove maybe
    fitnessCertExpiry :: Maybe UTCTime,
    permitNumber :: Maybe Text,
    permitStart :: Maybe UTCTime,
    permitExpiry :: Maybe UTCTime,
    pucExpiry :: Maybe UTCTime,
    vehicleClass :: Maybe COV,
    vehicleNumber :: Maybe Text,
    vehicleColor :: Maybe Text,
    vehicleManufacturer :: Maybe Text,
    vehicleModel :: Maybe Text,
    insuranceValidity :: Maybe UTCTime,
    idfyRequestId :: Text,
    verificationRespDump :: Text,
    idfyStatus :: IdfyStatus,
    verificationStatus :: VerificationStatus,
    rcImageS3Path :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    consent :: Bool,
    consentTimestamp :: UTCTime
  }
  deriving (Generic)

type VehicleRegistrationCert = VehicleRegistrationCertE 'AsEncrypted

type DecryptedVehicleRegistrationCert = VehicleRegistrationCertE 'AsUnencrypted

deriving instance Show DecryptedVehicleRegistrationCert

instance EncryptedItem VehicleRegistrationCert where
  type Unencrypted VehicleRegistrationCert = (DecryptedVehicleRegistrationCert, HashSalt)
  encryptItem (VehicleRegistrationCert {..}, salt) = do
    vehicleRegistrationCertNumber_ <- encryptItem $ (,salt) <$> vehicleRegistrationCertNumber
    return VehicleRegistrationCert {vehicleRegistrationCertNumber = vehicleRegistrationCertNumber_, ..}
  decryptItem VehicleRegistrationCert {..} = do
    vehicleRegistrationCertNumber_ <- fmap fst <$> decryptItem vehicleRegistrationCertNumber
    return (VehicleRegistrationCert {vehicleRegistrationCertNumber = vehicleRegistrationCertNumber_, ..}, "")

instance EncryptedItem' VehicleRegistrationCert where
  type UnencryptedItem VehicleRegistrationCert = DecryptedVehicleRegistrationCert
  toUnencrypted a salt = (a, salt)
  fromUnencrypted a = fst a

data VehicleRegistrationCertAPIEntity = VehicleRegistrationCertAPIEntity
  { id :: Id VehicleRegistrationCert,
    driverId :: Id Person,
    vehicleRegistrationCertNumber :: Maybe Text,
    fitnessCertExpiry :: Maybe UTCTime,
    permitNumber :: Maybe Text,
    permitStart :: Maybe UTCTime,
    permitExpiry :: Maybe UTCTime,
    pucExpiry :: Maybe UTCTime,
    vehicleClass :: Maybe COV,
    vehicleNumber :: Maybe Text,
    vehicleColor :: Maybe Text,
    vehicleManufacturer :: Maybe Text,
    vehicleModel :: Maybe Text,
    insuranceValidity :: Maybe UTCTime,
    verificationStatus :: VerificationStatus,
    idfyStatus :: IdfyStatus
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

makePersonAPIEntity :: DecryptedVehicleRegistrationCert -> VehicleRegistrationCertAPIEntity
makePersonAPIEntity VehicleRegistrationCert {..} =
  VehicleRegistrationCertAPIEntity
    { vehicleRegistrationCertNumber = maskText <$> vehicleRegistrationCertNumber,
      ..
    }
