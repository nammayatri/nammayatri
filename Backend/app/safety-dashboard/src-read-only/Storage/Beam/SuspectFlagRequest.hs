{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.SuspectFlagRequest where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import qualified Domain.Types.SuspectFlagRequest
import qualified Kernel.Prelude
import qualified Domain.Types.Suspect
import qualified Database.Beam as B



data SuspectFlagRequestT f
    = SuspectFlagRequestT {adminApproval :: (B.C f Domain.Types.SuspectFlagRequest.AdminApproval),
                           approvedBy :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                           createdAt :: (B.C f Kernel.Prelude.UTCTime),
                           dl :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                           firstName :: (B.C f Kernel.Prelude.Text),
                           flaggedBy :: (B.C f Kernel.Prelude.Text),
                           flaggedCategory :: (B.C f Kernel.Prelude.Text),
                           flaggedReason :: (B.C f Kernel.Prelude.Text),
                           flaggedStatus :: (B.C f Domain.Types.Suspect.FlaggedStatus),
                           id :: (B.C f Kernel.Prelude.Text),
                           lastName :: (B.C f Kernel.Prelude.Text),
                           merchantShortId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                           mobileNumber :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                           reportDetails :: (B.C f (Kernel.Prelude.Maybe Domain.Types.Suspect.ReportDetails)),
                           totalComplaintsCount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                           updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                           voterId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                           merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))}
    deriving (Generic, B.Beamable)
instance B.Table SuspectFlagRequestT
    where data PrimaryKey SuspectFlagRequestT f = SuspectFlagRequestId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = SuspectFlagRequestId . id
type SuspectFlagRequest = SuspectFlagRequestT Identity

$(enableKVPG (''SuspectFlagRequestT) [('id)] [])

$(mkTableInstances (''SuspectFlagRequestT) "suspect_flag_request")

