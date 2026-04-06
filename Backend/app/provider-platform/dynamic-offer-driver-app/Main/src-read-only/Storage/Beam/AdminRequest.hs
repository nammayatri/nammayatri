{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.AdminRequest where

import qualified Database.Beam as B
import qualified Domain.Types.AdminRequest
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data AdminRequestT f = AdminRequestT
  { actionType :: B.C f Domain.Types.AdminRequest.ActionType,
    adjustmentType :: B.C f (Kernel.Prelude.Maybe Domain.Types.AdminRequest.AdjustmentType),
    adminCheckerId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    adminCheckerName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    adminMakerId :: B.C f Kernel.Prelude.Text,
    adminMakerName :: B.C f Kernel.Prelude.Text,
    amount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    description :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    errorMessage :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    personId :: B.C f Kernel.Prelude.Text,
    referenceId :: B.C f Kernel.Prelude.Text,
    referenceTable :: B.C f Domain.Types.AdminRequest.ReferenceTable,
    referenceType :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    source :: B.C f (Kernel.Prelude.Maybe Domain.Types.AdminRequest.AdjustmentSource),
    status :: B.C f Domain.Types.AdminRequest.AdminRequestStatus,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table AdminRequestT where
  data PrimaryKey AdminRequestT f = AdminRequestId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = AdminRequestId . id

type AdminRequest = AdminRequestT Identity

$(enableKVPG ''AdminRequestT ['id] [['referenceId]])

$(mkTableInstances ''AdminRequestT "admin_request")
