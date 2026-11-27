module API.Internal.FleetScopeBackfill
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.FleetScopeBackfill as Domain
import Domain.Types.Merchant (Merchant)
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Tools.ActorInfo as ActorInfo

-- | Copy today's access into person_role_assignment for one merchant. Dry run unless @dryRun=false@.
type API =
  Capture "merchantId" (ShortId Merchant)
    :> "fleetScopeBackfill"
    :> QueryParam "dryRun" Bool
    :> Header "token" Text
    :> Post '[JSON] Domain.FleetScopeBackfillRes

handler :: FlowServer API
handler = fleetScopeBackfill

fleetScopeBackfill :: ShortId Merchant -> Maybe Bool -> Maybe Text -> FlowHandler Domain.FleetScopeBackfillRes
fleetScopeBackfill merchantShortId mbDryRun apiKey =
  withFlowHandlerAPI . ActorInfo.withRequestIdActorInfo $
    Domain.fleetScopeBackfill merchantShortId (fromMaybe True mbDryRun) apiKey
