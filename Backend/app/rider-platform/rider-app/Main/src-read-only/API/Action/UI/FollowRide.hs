{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.FollowRide
  ( API,
    handler,
  )
where

import qualified API.Types.UI.FollowRide
import qualified Control.Lens
import qualified Domain.Action.UI.FollowRide
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "follow" :> "ride" :> Get '[JSON] [API.Types.UI.FollowRide.Followers] :<|> TokenAuth :> "share" :> "ride"
      :> ReqBody
           '[JSON]
           API.Types.UI.FollowRide.ShareRideReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "followRide"
      :> "ECStatus"
      :> Capture
           "rideId"
           (Kernel.Types.Id.Id Domain.Types.Ride.Ride)
      :> Get
           '[JSON]
           API.Types.UI.FollowRide.EmergencyContactsStatusRes
      :<|> TokenAuth
      :> "followRide"
      :> Capture
           "rideId"
           (Kernel.Types.Id.Id Domain.Types.Ride.Ride)
      :> "customerDetails"
      :> Get
           '[JSON]
           API.Types.UI.FollowRide.FollowRideCustomerDetailsRes
  )

handler :: Environment.FlowServer API
handler = getFollowRide :<|> postShareRide :<|> getFollowRideECStatus :<|> getFollowRideCustomerDetails

getFollowRide :: ((Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.FlowHandler [API.Types.UI.FollowRide.Followers])
getFollowRide a1 = withFlowHandlerAPI $ Domain.Action.UI.FollowRide.getFollowRide (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

postShareRide ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.FollowRide.ShareRideReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postShareRide a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FollowRide.postShareRide (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getFollowRideECStatus ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    Environment.FlowHandler API.Types.UI.FollowRide.EmergencyContactsStatusRes
  )
getFollowRideECStatus a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FollowRide.getFollowRideECStatus (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getFollowRideCustomerDetails ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    Environment.FlowHandler API.Types.UI.FollowRide.FollowRideCustomerDetailsRes
  )
getFollowRideCustomerDetails a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FollowRide.getFollowRideCustomerDetails (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
