{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.OperationHubRequests where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.OperationHubRequests
import qualified Database.Beam as B



data OperationHubRequestsT f
    = OperationHubRequestsT {creatorId :: (B.C f Kernel.Prelude.Text),
                             driverId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                             fulfilledAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                             id :: (B.C f Kernel.Prelude.Text),
                             merchantId :: (B.C f Kernel.Prelude.Text),
                             merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                             operationHubId :: (B.C f Kernel.Prelude.Text),
                             operatorId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                             registrationNo :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                             remarks :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                             requestStatus :: (B.C f Domain.Types.OperationHubRequests.RequestStatus),
                             requestType :: (B.C f Domain.Types.OperationHubRequests.RequestType),
                             createdAt :: (B.C f Kernel.Prelude.UTCTime),
                             updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table OperationHubRequestsT
    where data PrimaryKey OperationHubRequestsT f = OperationHubRequestsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = OperationHubRequestsId . id
type OperationHubRequests = OperationHubRequestsT Identity

$(enableKVPG (''OperationHubRequestsT) [('id)] [])

$(mkTableInstances (''OperationHubRequestsT) "operation_hub_requests")

