module Domain.Types.Alert.AlertRequestStatus where

import Kernel.Prelude

data AlertRequestStatus = ACCEPTED | REJECTED | AWAITING_APPROVAL | REVOKED deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema), ToParamSchema)
