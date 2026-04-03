{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.BppDetails where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data BppDetailsT f
    = BppDetailsT {description :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                   domain :: (B.C f Kernel.Prelude.Text),
                   id :: (B.C f Kernel.Prelude.Text),
                   logoUrl :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                   name :: (B.C f Kernel.Prelude.Text),
                   subscriberId :: (B.C f Kernel.Prelude.Text),
                   supportNumber :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                   createdAt :: (B.C f Kernel.Prelude.UTCTime),
                   updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table BppDetailsT
    where data PrimaryKey BppDetailsT f = BppDetailsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = BppDetailsId . id
type BppDetails = BppDetailsT Identity

$(enableKVPG (''BppDetailsT) [('id)] [[('subscriberId)]])

$(mkTableInstances (''BppDetailsT) "bpp_details")

