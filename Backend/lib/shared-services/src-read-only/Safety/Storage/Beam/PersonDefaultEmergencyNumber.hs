{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Safety.Storage.Beam.PersonDefaultEmergencyNumber where
import Kernel.Prelude
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import qualified Kernel.Prelude
import qualified Safety.Domain.Types.Common
import qualified Kernel.External.Encryption
import qualified Database.Beam as B



data PersonDefaultEmergencyNumberT f
    = PersonDefaultEmergencyNumberT {contactPersonId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                                     createdAt :: (B.C f Kernel.Prelude.UTCTime),
                                     enableForFollowing :: (B.C f Kernel.Prelude.Bool),
                                     enableForShareRide :: (B.C f Kernel.Prelude.Bool),
                                     merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                                     mobileCountryCode :: (B.C f Kernel.Prelude.Text),
                                     mobileNumberEncrypted :: (B.C f Kernel.Prelude.Text),
                                     mobileNumberHash :: (B.C f Kernel.External.Encryption.DbHash),
                                     name :: (B.C f Kernel.Prelude.Text),
                                     personId :: (B.C f Kernel.Prelude.Text),
                                     priority :: (B.C f Kernel.Prelude.Int),
                                     shareTripWithEmergencyContactOption :: (B.C f (Kernel.Prelude.Maybe Safety.Domain.Types.Common.RideShareOptions))}
    deriving (Generic, B.Beamable)
instance B.Table PersonDefaultEmergencyNumberT
    where data PrimaryKey PersonDefaultEmergencyNumberT f = PersonDefaultEmergencyNumberId (B.C f Kernel.External.Encryption.DbHash) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = PersonDefaultEmergencyNumberId <$> mobileNumberHash <*> personId
type PersonDefaultEmergencyNumber = PersonDefaultEmergencyNumberT Identity

$(enableKVPG (''PersonDefaultEmergencyNumberT) [('mobileNumberHash), ('personId)] [[('contactPersonId)], [('mobileNumberHash)], [('personId)]])

$(mkTableInstancesGenericSchema (''PersonDefaultEmergencyNumberT) "person_default_emergency_number")

