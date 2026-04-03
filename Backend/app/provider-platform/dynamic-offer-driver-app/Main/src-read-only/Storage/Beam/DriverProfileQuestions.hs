{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.DriverProfileQuestions where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data DriverProfileQuestionsT f
    = DriverProfileQuestionsT {aboutMe :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                               aspirations :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text])),
                               createdAt :: (B.C f Kernel.Prelude.UTCTime),
                               driverId :: (B.C f Kernel.Prelude.Text),
                               drivingSince :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                               hometown :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                               imageIds :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text])),
                               merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                               pledges :: (B.C f [Kernel.Prelude.Text]),
                               updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                               vehicleTags :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]))}
    deriving (Generic, B.Beamable)
instance B.Table DriverProfileQuestionsT
    where data PrimaryKey DriverProfileQuestionsT f = DriverProfileQuestionsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = DriverProfileQuestionsId . driverId
type DriverProfileQuestions = DriverProfileQuestionsT Identity

$(enableKVPG (''DriverProfileQuestionsT) [('driverId)] [])

$(mkTableInstances (''DriverProfileQuestionsT) "driver_profile_questions")

