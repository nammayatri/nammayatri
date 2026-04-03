{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.CallFeedbackOptions where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data CallFeedbackOptionsT f
    = CallFeedbackOptionsT {category :: (B.C f Kernel.Prelude.Text),
                            id :: (B.C f Kernel.Prelude.Text),
                            messageKey :: (B.C f Kernel.Prelude.Text),
                            merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                            merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                            createdAt :: (B.C f Kernel.Prelude.UTCTime),
                            updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table CallFeedbackOptionsT
    where data PrimaryKey CallFeedbackOptionsT f = CallFeedbackOptionsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = CallFeedbackOptionsId . id
type CallFeedbackOptions = CallFeedbackOptionsT Identity

$(enableKVPG (''CallFeedbackOptionsT) [('id)] [])

$(mkTableInstances (''CallFeedbackOptionsT) "call_feedback_options")

