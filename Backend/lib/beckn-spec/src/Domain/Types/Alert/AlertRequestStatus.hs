module Domain.Types.Alert.AlertRequestStatus where

import Data.Aeson
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnum)
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)

data AlertRequestStatus = ACCEPTED | REJECTED | AWAITING_APPROVAL | REVOKED | TRIGGERED deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema), ToParamSchema)

$(mkBeamInstancesForEnum ''AlertRequestStatus)

$(mkHttpInstancesForEnum ''AlertRequestStatus)
