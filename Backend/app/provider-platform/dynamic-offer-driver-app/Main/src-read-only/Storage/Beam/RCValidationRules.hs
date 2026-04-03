{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.RCValidationRules where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data RCValidationRulesT f
    = RCValidationRulesT {fuelType :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text])),
                          id :: (B.C f Kernel.Prelude.Text),
                          maxVehicleAge :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                          merchantId :: (B.C f Kernel.Prelude.Text),
                          merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                          vehicleClass :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text])),
                          vehicleOEM :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text])),
                          createdAt :: (B.C f Kernel.Prelude.UTCTime),
                          updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table RCValidationRulesT
    where data PrimaryKey RCValidationRulesT f = RCValidationRulesId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = RCValidationRulesId . id
type RCValidationRules = RCValidationRulesT Identity

$(enableKVPG (''RCValidationRulesT) [('id)] [])

$(mkTableInstances (''RCValidationRulesT) "rc_validation_rules")

