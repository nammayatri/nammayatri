{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.ApprovalRequest where

import qualified Data.Text
import qualified Database.Beam as B
import qualified Domain.Types.ApprovalRequest
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data ApprovalRequestT f = ApprovalRequestT
  { body :: B.C f Data.Text.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    id :: B.C f Data.Text.Text,
    merchantId :: B.C f Data.Text.Text,
    merchantOperatingCityId :: B.C f Data.Text.Text,
    reason :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    requestData :: B.C f Domain.Types.ApprovalRequest.ApprovalRequestData,
    requesteeId :: B.C f Data.Text.Text,
    requestorId :: B.C f Data.Text.Text,
    status :: B.C f Domain.Types.ApprovalRequest.RequestStatus,
    title :: B.C f Data.Text.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table ApprovalRequestT where
  data PrimaryKey ApprovalRequestT f = ApprovalRequestId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = ApprovalRequestId . id

type ApprovalRequest = ApprovalRequestT Identity

$(enableKVPG ''ApprovalRequestT ['id] [['requesteeId]])

$(mkTableInstances ''ApprovalRequestT "approval_request")
