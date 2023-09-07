{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.Person where

import qualified Database.Beam as B
import qualified Domain.Types.Person as Domain
import Tools.Beam.UtilsTH
import Kernel.External.Encryption (DbHash)
import Kernel.External.Maps (Language)
import Kernel.External.Whatsapp.Interface.Types (OptApiMethods (..))
import Kernel.Prelude

data PersonT f = PersonT
  { id :: B.C f Text,
    firstName :: B.C f (Maybe Text),
    middleName :: B.C f (Maybe Text),
    lastName :: B.C f (Maybe Text),
    role :: B.C f Domain.Role,
    gender :: B.C f Domain.Gender,
    identifierType :: B.C f Domain.IdentifierType,
    emailEncrypted :: B.C f (Maybe Text),
    emailHash :: B.C f (Maybe DbHash),
    unencryptedMobileNumber :: B.C f (Maybe Text),
    mobileNumberEncrypted :: B.C f (Maybe Text),
    mobileNumberHash :: B.C f (Maybe DbHash),
    mobileCountryCode :: B.C f (Maybe Text),
    passwordHash :: B.C f (Maybe DbHash),
    identifier :: B.C f (Maybe Text),
    totalRatings :: B.C f Int,
    totalRatingScore :: B.C f Int,
    isValidRating :: B.C f Bool,
    language :: B.C f (Maybe Language),
    isNew :: B.C f Bool,
    enabled :: B.C f Bool,
    blocked :: B.C f Bool,
    deviceToken :: B.C f (Maybe Text),
    notificationToken :: B.C f (Maybe Text),
    description :: B.C f (Maybe Text),
    merchantId :: B.C f Text,
    whatsappNotificationEnrollStatus :: B.C f (Maybe OptApiMethods),
    createdAt :: B.C f UTCTime,
    blockedAt :: B.C f (Maybe LocalTime),
    blockedByRuleId :: B.C f (Maybe Text),
    aadhaarVerified :: B.C f Bool,
    updatedAt :: B.C f UTCTime,
    bundleVersion :: B.C f (Maybe Text),
    clientVersion :: B.C f (Maybe Text),
    hasTakenValidRide :: B.C f Bool,
    hasDisability :: B.C f (Maybe Bool),
    referralCode :: B.C f (Maybe Text),
    referredAt :: B.C f (Maybe UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table PersonT where
  data PrimaryKey PersonT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Person = PersonT Identity

$(enableKVPG ''PersonT ['id] [['mobileNumberHash], ['emailHash], ['referralCode], ['deviceToken]])

$(mkTableInstances ''PersonT "person")
