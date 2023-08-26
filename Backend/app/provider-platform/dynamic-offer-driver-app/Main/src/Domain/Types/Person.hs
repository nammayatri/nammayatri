{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Types.Person where

import Data.Aeson
import Data.OpenApi (ToSchema)
import Data.Time
import qualified Domain.Types.MediaFile as M
import qualified Domain.Types.Merchant as DM
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption
import qualified Kernel.External.Maps as Maps
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.External.Types (Language)
import qualified Kernel.External.Whatsapp.Interface.Types as Whatsapp (OptApiMethods)
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Common (Centesimal, maskText)
import Kernel.Utils.TH (mkFromHttpInstanceForEnum, mkHttpInstancesForEnum)
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data Role
  = DRIVER
  | ADMIN
  deriving stock (Show, Eq, Read, Ord, Enum, Bounded, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

$(mkBeamInstancesForEnum ''Role)

$(mkHttpInstancesForEnum ''Role)

-------------------------------------------------------------------------------------------
data IdentifierType = MOBILENUMBER | AADHAAR | EMAIL
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

$(mkBeamInstancesForEnum ''IdentifierType)

$(mkHttpInstancesForEnum ''IdentifierType)

--------------------------------------------------------------------------------------------------
data Gender = MALE | FEMALE | OTHER | UNKNOWN | PREFER_NOT_TO_SAY
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

$(mkBeamInstancesForEnum ''Gender)

$(mkFromHttpInstanceForEnum ''Gender)

data PersonE e = Person
  { id :: Id Person,
    firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    role :: Role,
    gender :: Gender,
    hometown :: Maybe Text,
    languagesSpoken :: Maybe [Text],
    identifierType :: IdentifierType,
    email :: Maybe Text,
    unencryptedMobileNumber :: Maybe Text,
    mobileNumber :: Maybe (EncryptedHashedField e Text),
    mobileCountryCode :: Maybe Text,
    passwordHash :: Maybe DbHash,
    identifier :: Maybe Text,
    rating :: Maybe Centesimal,
    isNew :: Bool,
    onboardedFromDashboard :: Bool,
    merchantId :: Id DM.Merchant,
    deviceToken :: Maybe FCM.FCMRecipientToken,
    whatsappNotificationEnrollStatus :: Maybe Whatsapp.OptApiMethods,
    language :: Maybe Language,
    description :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    bundleVersion :: Maybe Version,
    clientVersion :: Maybe Version,
    unencryptedAlternateMobileNumber :: Maybe Text,
    alternateMobileNumber :: Maybe (EncryptedHashedField e Text),
    faceImageId :: Maybe (Id M.MediaFile)
  }
  deriving (Generic)

type Person = PersonE 'AsEncrypted

type Driver = Person

type DecryptedPerson = PersonE 'AsUnencrypted

instance EncryptedItem Person where
  type Unencrypted Person = (DecryptedPerson, HashSalt)
  encryptItem (Person {..}, salt) = do
    mobileNumber_ <- encryptItem $ (,salt) <$> mobileNumber
    alternateMobileNumber_ <- encryptItem $ (,salt) <$> alternateMobileNumber
    return Person {mobileNumber = mobileNumber_, alternateMobileNumber = alternateMobileNumber_, ..}
  decryptItem Person {..} = do
    mobileNumber_ <- fmap fst <$> decryptItem mobileNumber
    alternateMobileNumber_ <- fmap fst <$> decryptItem alternateMobileNumber
    return (Person {mobileNumber = mobileNumber_, alternateMobileNumber = alternateMobileNumber_, ..}, "")

instance EncryptedItem' Person where
  type UnencryptedItem Person = DecryptedPerson
  toUnencrypted a salt = (a, salt)
  fromUnencrypted a = fst a

data PersonAPIEntity = PersonAPIEntity
  { id :: Id Person,
    firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    maskedMobileNumber :: Maybe Text,
    maskedDeviceToken :: Maybe FCM.FCMRecipientToken,
    whatsappNotificationEnrollStatus :: Maybe Whatsapp.OptApiMethods,
    role :: Role,
    language :: Maybe Maps.Language
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

makePersonAPIEntity :: DecryptedPerson -> PersonAPIEntity
makePersonAPIEntity Person {..} =
  PersonAPIEntity
    { maskedMobileNumber = maskText <$> mobileNumber,
      maskedDeviceToken = FCM.FCMRecipientToken . maskText . (.getFCMRecipientToken) <$> deviceToken,
      ..
    }

getPersonNumber :: (EncFlow m r) => Person -> m (Maybe Text)
getPersonNumber person = do
  mapM decrypt person.mobileNumber

getPersonFullName :: Person -> Maybe Text
getPersonFullName person = (\fN -> fN <> maybe "" (" " <>) person.lastName) <$> Just person.firstName

roundToOneDecimal :: Centesimal -> Centesimal
roundToOneDecimal x = fromIntegral (round @Centesimal @Int (x * 10)) / 10
