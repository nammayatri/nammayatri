{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.ReviewRequest where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.ReviewRequest
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data ReviewRequestT f = ReviewRequestT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    documentDetails :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    entityId :: B.C f Kernel.Prelude.Text,
    entityType :: B.C f Domain.Types.ReviewRequest.EntityType,
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    rcNo :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    requestStatus :: B.C f Domain.Types.ReviewRequest.RequestStatus,
    requestType :: B.C f Domain.Types.ReviewRequest.RequestType,
    reviewerId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table ReviewRequestT where
  data PrimaryKey ReviewRequestT f = ReviewRequestId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = ReviewRequestId . id

type ReviewRequest = ReviewRequestT Identity

$(enableKVPG ''ReviewRequestT ['id] [])

$(mkTableInstances ''ReviewRequestT "review_request")
