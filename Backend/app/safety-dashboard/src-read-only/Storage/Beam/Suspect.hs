{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.Suspect where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import qualified Kernel.Prelude
import qualified Domain.Types.Suspect
import qualified Data.Aeson
import qualified Database.Beam as B



data SuspectT f
    = SuspectT {createdAt :: (B.C f Kernel.Prelude.UTCTime),
                dl :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                firstName :: (B.C f Kernel.Prelude.Text),
                flagUpdatedAt :: (B.C f Kernel.Prelude.UTCTime),
                flaggedBy :: (B.C f Data.Aeson.Value),
                flaggedCounter :: (B.C f Kernel.Prelude.Int),
                flaggedStatus :: (B.C f Domain.Types.Suspect.FlaggedStatus),
                id :: (B.C f Kernel.Prelude.Text),
                lastName :: (B.C f Kernel.Prelude.Text),
                statusChangedReason :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                voterId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text))}
    deriving (Generic, B.Beamable)
instance B.Table SuspectT
    where data PrimaryKey SuspectT f = SuspectId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = SuspectId . id
type Suspect = SuspectT Identity

$(enableKVPG (''SuspectT) [('id)] [])

$(mkTableInstances (''SuspectT) "suspect")

