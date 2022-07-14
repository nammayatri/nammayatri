{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.Driveronboarding.VehicleRegistrationCert where
import Beckn.Prelude
import Beckn.Types.Id
import Domain.Types.Person (Person)
import Data.Aeson
import Data.OpenApi
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Beckn.Utils.JSON (constructorsToLowerOptions)
import Beckn.External.Encryption
import Beckn.Utils.Common

data VerificationStatus = PENDING | VALID | INVALID
  deriving (Show, Eq, Read, Generic, Enum, Bounded, FromJSON, ToJSON, ToSchema)

-- added valid and invalid inorder to accomodate validation results
data IdfyStatus = IN_PROGRESS |  FAILED | COMPLETED
  deriving (Show,Eq,Read,Generic,Enum,Bounded)
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
    { constructorTagModifier =  \case
        "3W_NT" -> "W_NT"
        "3W_T" -> "W_T"
        "3W_CAB" -> "W_CAB"
        val -> val
    } 

constructorForCOVToJson :: Options
constructorForCOVToJson =
  defaultOptions
    { constructorTagModifier =  \case
        "W_NT" -> "3W_NT"
        "W_T" -> "3W_T"
        "W_CAB" -> "3W_CAB"
        val -> val
    }       
-- here we should only check vehicle class with three wheeler vehicle type only

data VehicleRegistrationCertE e = VehicleRegistrationCert {
    id :: Id VehicleRegistrationCert,
    driverId :: Id Person,
    vehicleRegistrationCertNumber :: Maybe (EncryptedHashedField e Text), -- remove maybe
    fitnessCertExpiry :: Maybe UTCTime,
    permitNumber :: Maybe Text,
    permitStart :: Maybe UTCTime,
    permitExpiry :: Maybe UTCTime,
    vehicleClass :: Maybe COV,
    vehicleNumber :: Maybe Text,
    insuranceValidity :: Maybe UTCTime,
    request_id :: Text,
    idfyStatus :: IdfyStatus,
    verificationStatus :: VerificationStatus,
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
  { 
    id :: Id VehicleRegistrationCert,
    driverId :: Id Person,
    vehicleRegistrationCertNumber :: Maybe Text,
    fitnessCertExpiry :: Maybe UTCTime,
    permitNumber :: Maybe Text,
    permitStart :: Maybe UTCTime,
    permitExpiry :: Maybe UTCTime,
    vehicleClass :: Maybe COV,
    vehicleNumber :: Maybe Text,
    insuranceValidity :: Maybe UTCTime,
    request_id :: Text,
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






