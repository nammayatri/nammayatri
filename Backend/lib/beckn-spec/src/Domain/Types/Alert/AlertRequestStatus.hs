module Domain.Types.Alert.AlertRequestStatus where

import Kernel.Prelude
import Servant.API (ToHttpApiData (..))

data AlertRequestStatus = ACCEPTED | REJECTED | AWAITING_APPROVAL | REVOKED deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema), ToParamSchema)

instance ToHttpApiData AlertRequestStatus where
  toUrlPiece ACCEPTED = "ACCEPTED"
  toUrlPiece REJECTED = "REJECTED"
  toUrlPiece AWAITING_APPROVAL = "AWAITING_APPROVAL"
  toUrlPiece REVOKED = "REVOKED"
