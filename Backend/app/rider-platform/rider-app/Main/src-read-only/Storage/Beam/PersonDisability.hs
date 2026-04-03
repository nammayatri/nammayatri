{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.PersonDisability where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data PersonDisabilityT f
    = PersonDisabilityT {createdAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                         description :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                         disabilityId :: (B.C f Kernel.Prelude.Text),
                         personId :: (B.C f Kernel.Prelude.Text),
                         tag :: (B.C f Kernel.Prelude.Text),
                         updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table PersonDisabilityT
    where data PrimaryKey PersonDisabilityT f = PersonDisabilityId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = PersonDisabilityId . personId
type PersonDisability = PersonDisabilityT Identity

$(enableKVPG (''PersonDisabilityT) [('personId)] [])

$(mkTableInstances (''PersonDisabilityT) "person_disability")

