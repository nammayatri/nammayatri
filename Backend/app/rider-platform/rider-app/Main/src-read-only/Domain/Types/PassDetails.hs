{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PassDetails where

import Data.Aeson
import qualified Data.Time
import qualified Data.Time.Calendar
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PassOrganization
import qualified Domain.Types.PassType
import qualified Domain.Types.Person
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import qualified Kernel.External.Maps.Google.MapsClient.Types
import qualified Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data PassDetailsE e = PassDetails
  { aadharNo :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    academicYearEnd :: Kernel.Prelude.Maybe Data.Time.Calendar.Day,
    academicYearStart :: Kernel.Prelude.Maybe Data.Time.Calendar.Day,
    address :: Kernel.Prelude.Maybe Domain.Types.PassDetails.PassDetailAddress,
    age :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    applicableRouteIds :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    createdAt :: Data.Time.UTCTime,
    department :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    gender :: Domain.Types.Person.Gender,
    guardianMobileNumber :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    guardianName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.PassDetails.PassDetails,
    idCardPicture :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    name :: Kernel.Prelude.Text,
    numberOfStages :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    passEnum :: Domain.Types.PassType.PassEnum,
    passOrganizationId :: Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    pincode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    referenceNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    registerNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    remark :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    routePairs :: [Domain.Types.PassDetails.RoutePair],
    selfImage :: Kernel.Prelude.Text,
    updatedAt :: Data.Time.UTCTime,
    validTill :: Data.Time.UTCTime,
    verificationStatus :: Domain.Types.PassDetails.VerificationStatus,
    year :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic)

type PassDetails = PassDetailsE 'AsEncrypted

type DecryptedPassDetails = PassDetailsE 'AsUnencrypted

instance EncryptedItem PassDetails where
  type Unencrypted PassDetails = (DecryptedPassDetails, HashSalt)
  encryptItem (entity, salt) = do
    aadharNo_ <- encryptItem $ (,salt) <$> aadharNo entity
    guardianMobileNumber_ <- encryptItem $ (,salt) <$> guardianMobileNumber entity
    pure
      PassDetails
        { aadharNo = aadharNo_,
          academicYearEnd = academicYearEnd entity,
          academicYearStart = academicYearStart entity,
          address = address entity,
          age = age entity,
          applicableRouteIds = applicableRouteIds entity,
          createdAt = createdAt entity,
          department = department entity,
          gender = gender entity,
          guardianMobileNumber = guardianMobileNumber_,
          guardianName = guardianName entity,
          id = id entity,
          idCardPicture = idCardPicture entity,
          merchantId = merchantId entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          name = name entity,
          numberOfStages = numberOfStages entity,
          passEnum = passEnum entity,
          passOrganizationId = passOrganizationId entity,
          personId = personId entity,
          pincode = pincode entity,
          referenceNumber = referenceNumber entity,
          registerNo = registerNo entity,
          remark = remark entity,
          routePairs = routePairs entity,
          selfImage = selfImage entity,
          updatedAt = updatedAt entity,
          validTill = validTill entity,
          verificationStatus = verificationStatus entity,
          year = year entity
        }
  decryptItem entity = do
    aadharNo_ <- fmap fst <$> decryptItem (aadharNo entity)
    guardianMobileNumber_ <- fmap fst <$> decryptItem (guardianMobileNumber entity)
    pure
      ( PassDetails
          { aadharNo = aadharNo_,
            academicYearEnd = academicYearEnd entity,
            academicYearStart = academicYearStart entity,
            address = address entity,
            age = age entity,
            applicableRouteIds = applicableRouteIds entity,
            createdAt = createdAt entity,
            department = department entity,
            gender = gender entity,
            guardianMobileNumber = guardianMobileNumber_,
            guardianName = guardianName entity,
            id = id entity,
            idCardPicture = idCardPicture entity,
            merchantId = merchantId entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            name = name entity,
            numberOfStages = numberOfStages entity,
            passEnum = passEnum entity,
            passOrganizationId = passOrganizationId entity,
            personId = personId entity,
            pincode = pincode entity,
            referenceNumber = referenceNumber entity,
            registerNo = registerNo entity,
            remark = remark entity,
            routePairs = routePairs entity,
            selfImage = selfImage entity,
            updatedAt = updatedAt entity,
            validTill = validTill entity,
            verificationStatus = verificationStatus entity,
            year = year entity
          },
        ""
      )

instance EncryptedItem' PassDetails where
  type UnencryptedItem PassDetails = DecryptedPassDetails
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst

data PassDetailAddress = PassDetailAddress {latLng :: Kernel.External.Maps.Google.MapsClient.Types.LatLngV2, placeName :: Kernel.Prelude.Text}
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data RoutePair = RoutePair
  { destLatLong :: Kernel.External.Maps.Types.LatLong,
    destStopId :: Kernel.Prelude.Text,
    destStopName :: Kernel.Prelude.Text,
    srcLatLong :: Kernel.External.Maps.Types.LatLong,
    srcStopId :: Kernel.Prelude.Text,
    srcStopName :: Kernel.Prelude.Text
  }
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data VerificationStatus = PENDING | CLG_VERIFIED | MTC_VERIFIED | REJECTED | EXPIRED deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''VerificationStatus)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''VerificationStatus)
