{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.Rating where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data RatingT f
    = RatingT {createdAt :: (B.C f Kernel.Prelude.UTCTime),
               feedbackDetails :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
               id :: (B.C f Kernel.Prelude.Text),
               mediaId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
               ratingValue :: (B.C f Kernel.Prelude.Int),
               rideId :: (B.C f Kernel.Prelude.Text),
               riderId :: (B.C f Kernel.Prelude.Text),
               updatedAt :: (B.C f Kernel.Prelude.UTCTime),
               wasOfferedAssistance :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
               merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
               merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))}
    deriving (Generic, B.Beamable)
instance B.Table RatingT
    where data PrimaryKey RatingT f = RatingId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = RatingId . id
type Rating = RatingT Identity

$(enableKVPG (''RatingT) [('id)] [[('rideId)]])

$(mkTableInstances (''RatingT) "rating")

