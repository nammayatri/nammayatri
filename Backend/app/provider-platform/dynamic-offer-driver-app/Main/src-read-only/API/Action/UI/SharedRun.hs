{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.SharedRun
  ( API,
    handler,
  )
where

import qualified API.Types.UI.SharedRun
import qualified Control.Lens
import qualified Domain.Action.UI.SharedRun
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
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
import qualified Tools.ActorInfo
import Tools.Auth

type API =
  ( TokenAuth
      :> ( "sharedRun"
             :> ( Capture "rideId" (Kernel.Types.Id.Id Domain.Types.Ride.Ride)
                    :> ( "acknowledge"
                           :> Post
                                ('[JSON])
                                Kernel.Types.APISuccess.APISuccess
                       )
                )
         )
  )
    :<|> ( TokenAuth
             :> ( "sharedRun"
                    :> ( Capture "rideId" (Kernel.Types.Id.Id Domain.Types.Ride.Ride)
                           :> ( "details"
                                  :> Get
                                       ('[JSON])
                                       API.Types.UI.SharedRun.SharedRunDetailsRes
                              )
                       )
                )
         )

handler :: Environment.FlowServer API
handler = postSharedRunAcknowledge :<|> getSharedRunDetails

postSharedRunAcknowledge ::
  ( Kernel.Types.Id.Id Domain.Types.Person.Person,
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  (Kernel.Types.Id.Id Domain.Types.Ride.Ride -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postSharedRunAcknowledge a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.SharedRun.postSharedRunAcknowledge (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getSharedRunDetails ::
  ( Kernel.Types.Id.Id Domain.Types.Person.Person,
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  (Kernel.Types.Id.Id Domain.Types.Ride.Ride -> Environment.FlowHandler API.Types.UI.SharedRun.SharedRunDetailsRes)
getSharedRunDetails a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.SharedRun.getSharedRunDetails (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
