{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PassDetails where

import qualified Data.Aeson
import qualified Data.Time
import qualified Data.Time.Calendar
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.PassDetails
import qualified Domain.Types.PassType
import qualified Domain.Types.Person
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data PassDetailsT f = PassDetailsT
  { aadharNoEncrypted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    aadharNoHash :: B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash),
    academicYearEnd :: B.C f (Kernel.Prelude.Maybe Data.Time.Calendar.Day),
    academicYearStart :: B.C f (Kernel.Prelude.Maybe Data.Time.Calendar.Day),
    address :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    age :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    applicableRouteIds :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    createdAt :: B.C f Data.Time.UTCTime,
    department :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    gender :: B.C f Domain.Types.Person.Gender,
    guardianMobileNumberEncrypted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    guardianMobileNumberHash :: B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash),
    guardianName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    idCardPicture :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    name :: B.C f Kernel.Prelude.Text,
    numberOfStages :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    passEnum :: B.C f Domain.Types.PassType.PassEnum,
    passOrganizationId :: B.C f Kernel.Prelude.Text,
    personId :: B.C f Kernel.Prelude.Text,
    pincode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    referenceNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    registerNo :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    remark :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    routePairs :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    selfImage :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Data.Time.UTCTime,
    validTill :: B.C f Data.Time.UTCTime,
    verificationStatus :: B.C f Domain.Types.PassDetails.VerificationStatus,
    year :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table PassDetailsT where
  data PrimaryKey PassDetailsT f = PassDetailsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PassDetailsId . id

type PassDetails = PassDetailsT Identity

$(enableKVPG ''PassDetailsT ['id] [['passOrganizationId], ['personId]])

$(mkTableInstances ''PassDetailsT "pass_details")
