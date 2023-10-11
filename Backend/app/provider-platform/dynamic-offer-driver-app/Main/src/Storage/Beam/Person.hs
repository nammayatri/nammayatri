{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.Person where

import qualified Database.Beam as B
import qualified Domain.Types.Person as Domain
import Kernel.External.Encryption (DbHash (..))
import Kernel.External.Notification.FCM.Types (FCMRecipientToken (..))
import Kernel.External.Types (Language)
import Kernel.External.Whatsapp.Interface.Types (OptApiMethods (..))
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import Tools.Beam.UtilsTH

data PersonT f = PersonT
  { id :: B.C f Text,
    firstName :: B.C f Text,
    middleName :: B.C f (Maybe Text),
    lastName :: B.C f (Maybe Text),
    role :: B.C f Domain.Role,
    gender :: B.C f Domain.Gender,
    hometown :: B.C f (Maybe Text),
    languagesSpoken :: B.C f (Maybe [Text]),
    identifierType :: B.C f Domain.IdentifierType,
    email :: B.C f (Maybe Text),
    unencryptedMobileNumber :: B.C f (Maybe Text),
    mobileNumberEncrypted :: B.C f (Maybe Text),
    mobileNumberHash :: B.C f (Maybe DbHash),
    mobileCountryCode :: B.C f (Maybe Text),
    passwordHash :: B.C f (Maybe DbHash),
    identifier :: B.C f (Maybe Text),
    rating :: B.C f (Maybe Centesimal),
    isNew :: B.C f Bool,
    onboardedFromDashboard :: B.C f Bool,
    merchantId :: B.C f Text,
    merchantOperatingCityId :: B.C f (Maybe Text),
    deviceToken :: B.C f (Maybe FCMRecipientToken),
    language :: B.C f (Maybe Language),
    whatsappNotificationEnrollStatus :: B.C f (Maybe OptApiMethods),
    description :: B.C f (Maybe Text),
    alternateMobileNumberEncrypted :: B.C f (Maybe Text),
    unencryptedAlternateMobileNumber :: B.C f (Maybe Text),
    alternateMobileNumberHash :: B.C f (Maybe DbHash),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime,
    bundleVersion :: B.C f (Maybe Text),
    clientVersion :: B.C f (Maybe Text),
    faceImageId :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table PersonT where
  data PrimaryKey PersonT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Person = PersonT Identity

$(enableKVPG ''PersonT ['id] [['mobileNumberHash]]) -- DON'T Enable for KV

$(mkTableInstances ''PersonT "person")
