{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.OperationHubRequests where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.OperationHubRequests
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data OperationHubRequestsT f = OperationHubRequestsT
  { creatorId :: B.C f Kernel.Prelude.Text,
    driverId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fulfilledAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    operationHubId :: B.C f Kernel.Prelude.Text,
    operatorId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    registrationNo :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    remarks :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    requestStatus :: B.C f Domain.Types.OperationHubRequests.RequestStatus,
    requestType :: B.C f Domain.Types.OperationHubRequests.RequestType,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table OperationHubRequestsT where
  data PrimaryKey OperationHubRequestsT f = OperationHubRequestsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = OperationHubRequestsId . id

type OperationHubRequests = OperationHubRequestsT Identity

$(enableKVPG ''OperationHubRequestsT ['id] [])

$(mkTableInstances ''OperationHubRequestsT "operation_hub_requests")
