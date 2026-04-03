{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.AlertRequest where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Data.Text
import qualified Kernel.Prelude
import qualified Domain.Types.AlertRequest
import qualified Domain.Types.Alert.AlertRequestData
import qualified Domain.Types.Alert.AlertRequestType
import qualified Domain.Types.Alert.AlertRequestStatus
import qualified Database.Beam as B



data AlertRequestT f
    = AlertRequestT {body :: (B.C f Data.Text.Text),
                     createdAt :: (B.C f Kernel.Prelude.UTCTime),
                     id :: (B.C f Data.Text.Text),
                     merchantId :: (B.C f Data.Text.Text),
                     merchantOperatingCityId :: (B.C f Data.Text.Text),
                     reason :: (B.C f (Kernel.Prelude.Maybe Data.Text.Text)),
                     requestData :: (B.C f Domain.Types.Alert.AlertRequestData.AlertRequestData),
                     requestType :: (B.C f (Kernel.Prelude.Maybe Domain.Types.Alert.AlertRequestType.AlertRequestType)),
                     requesteeId :: (B.C f Data.Text.Text),
                     requesteeType :: (B.C f (Kernel.Prelude.Maybe Domain.Types.AlertRequest.RequesteeType)),
                     requestorId :: (B.C f Data.Text.Text),
                     requestorType :: (B.C f (Kernel.Prelude.Maybe Domain.Types.AlertRequest.RequestorType)),
                     status :: (B.C f Domain.Types.Alert.AlertRequestStatus.AlertRequestStatus),
                     title :: (B.C f Data.Text.Text),
                     updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table AlertRequestT
    where data PrimaryKey AlertRequestT f = AlertRequestId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
          primaryKey = AlertRequestId . id
type AlertRequest = AlertRequestT Identity

$(enableKVPG (''AlertRequestT) [('id)] [[('requesteeId)], [('requestorId)]])

$(mkTableInstances (''AlertRequestT) "approval_request")

