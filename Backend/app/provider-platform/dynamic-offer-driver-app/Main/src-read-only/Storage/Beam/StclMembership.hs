{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.StclMembership where

import qualified Data.Time
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.StclMembership
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data StclMembershipT f = StclMembershipT
  { aadharNumberEncrypted :: (B.C f Kernel.Prelude.Text),
    aadharNumberHash :: (B.C f Kernel.External.Encryption.DbHash),
    accountNumberEncrypted :: (B.C f Kernel.Prelude.Text),
    accountNumberHash :: (B.C f Kernel.External.Encryption.DbHash),
    addressCity :: (B.C f Kernel.Prelude.Text),
    addressPostalCode :: (B.C f Kernel.Prelude.Text),
    addressState :: (B.C f Kernel.Prelude.Text),
    addressStreetAddress1 :: (B.C f Kernel.Prelude.Text),
    addressStreetAddress2 :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    applicationId :: (B.C f Kernel.Prelude.Text),
    bankBranch :: (B.C f Kernel.Prelude.Text),
    bankName :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    dateOfBirth :: (B.C f Data.Time.Day),
    declarationDate :: (B.C f Data.Time.Day),
    declarationPlace :: (B.C f Kernel.Prelude.Text),
    declarationSignature :: (B.C f Kernel.Prelude.Text),
    driverId :: (B.C f Kernel.Prelude.Text),
    emailId :: (B.C f Kernel.Prelude.Text),
    fatherMotherName :: (B.C f Kernel.Prelude.Text),
    firstName :: (B.C f Kernel.Prelude.Text),
    fuelTypes :: (B.C f [Kernel.Prelude.Text]),
    id :: (B.C f Kernel.Prelude.Text),
    ifscCodeEncrypted :: (B.C f Kernel.Prelude.Text),
    ifscCodeHash :: (B.C f Kernel.External.Encryption.DbHash),
    lastName :: (B.C f Kernel.Prelude.Text),
    memberCategory :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    mobileNumberEncrypted :: (B.C f Kernel.Prelude.Text),
    mobileNumberHash :: (B.C f Kernel.External.Encryption.DbHash),
    nomineeAadharEncrypted :: (B.C f Kernel.Prelude.Text),
    nomineeAadharHash :: (B.C f Kernel.External.Encryption.DbHash),
    nomineeName :: (B.C f Kernel.Prelude.Text),
    numberOfShares :: (B.C f Kernel.Prelude.Int),
    panNumberEncrypted :: (B.C f Kernel.Prelude.Text),
    panNumberHash :: (B.C f Kernel.External.Encryption.DbHash),
    status :: (B.C f Domain.Types.StclMembership.ApplicationStatus),
    termsAccepted :: (B.C f Kernel.Prelude.Bool),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    vehicleType :: (B.C f Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table StclMembershipT where
  data PrimaryKey StclMembershipT f = StclMembershipId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = StclMembershipId . id

type StclMembership = StclMembershipT Identity

$(enableKVPG (''StclMembershipT) [('id)] [[('aadharNumberHash)], [('accountNumberHash)], [('applicationId)], [('driverId)], [('mobileNumberHash)], [('panNumberHash)]])

$(mkTableInstances (''StclMembershipT) "stcl_membership")
