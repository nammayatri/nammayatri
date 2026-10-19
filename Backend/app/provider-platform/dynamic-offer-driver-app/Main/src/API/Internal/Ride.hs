module API.Internal.Ride
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.Ride as Domain
import Domain.Types.Ride
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  ( "ride"
      :> Capture "rideId" (Id Ride)
      :> "deliveryImage"
      :> Header "token" Text
      :> Get '[JSON] Text
      :<|> "ride"
        :> Capture "rideId" (Id Ride)
        :> "evidence"
        :> Header "token" Text
        :> Get '[JSON] [Domain.RideEvidenceInternalItem]
      :<|> "ride"
        :> Capture "rideId" (Id Ride)
        :> "evidence"
        :> Capture "evidenceId" Text
        :> "file"
        :> Header "token" Text
        :> Get '[JSON] Text
  )

handler :: FlowServer API
handler =
  getDeliveryImage
    :<|> getRideEvidence
    :<|> getRideEvidenceFile

getDeliveryImage :: Id Ride -> Maybe Text -> FlowHandler Text
getDeliveryImage rideId = withFlowHandlerAPI . Domain.getDeliveryImage rideId

getRideEvidence :: Id Ride -> Maybe Text -> FlowHandler [Domain.RideEvidenceInternalItem]
getRideEvidence rideId = withFlowHandlerAPI . Domain.getRideEvidence rideId

getRideEvidenceFile :: Id Ride -> Text -> Maybe Text -> FlowHandler Text
getRideEvidenceFile rideId evidenceId = withFlowHandlerAPI . Domain.getRideEvidenceFile rideId evidenceId
