{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Person where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Person
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import qualified Kernel.External.Notification.FCM.Types
import qualified Kernel.External.Types
import qualified Kernel.External.Whatsapp.Interface.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Version
import Tools.Beam.UtilsTH

data PersonT f = PersonT
  { alternateMobileNumberEncrypted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    alternateMobileNumberHash :: B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash),
    backendAppVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    backendConfigVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientBundleVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientConfigVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientManufacturer :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientModelName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientOsType :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Version.DeviceType),
    clientOsVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientSdkVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    description :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    deviceToken :: B.C f (Kernel.Prelude.Maybe Kernel.External.Notification.FCM.Types.FCMRecipientToken),
    driverTag :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    email :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    faceImageId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    firstName :: B.C f Kernel.Prelude.Text,
    gender :: B.C f Domain.Types.Person.Gender,
    hometown :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    identifier :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    identifierType :: B.C f Domain.Types.Person.IdentifierType,
    isNew :: B.C f Kernel.Prelude.Bool,
    language :: B.C f (Kernel.Prelude.Maybe Kernel.External.Types.Language),
    languagesSpoken :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    lastName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    middleName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    mobileCountryCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    mobileNumberEncrypted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    mobileNumberHash :: B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash),
    onboardedFromDashboard :: B.C f Kernel.Prelude.Bool,
    passwordHash :: B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash),
    registrationLat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    registrationLon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    role :: B.C f Domain.Types.Person.Role,
    totalEarnedCoins :: B.C f Kernel.Prelude.Int,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    useFakeOtp :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    usedCoins :: B.C f Kernel.Prelude.Int,
    whatsappNotificationEnrollStatus :: B.C f (Kernel.Prelude.Maybe Kernel.External.Whatsapp.Interface.Types.OptApiMethods)
  }
  deriving (Generic, B.Beamable)

instance B.Table PersonT where
  data PrimaryKey PersonT f = PersonId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PersonId . id

type Person = PersonT Identity

$(enableKVPG ''PersonT ['id] [['alternateMobileNumberHash], ['mobileNumberHash]])

$(mkTableInstances ''PersonT "person")
