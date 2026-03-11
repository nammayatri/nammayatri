{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.CancellationReasonLookup
  ( API,
    handler,
  )
where

import qualified API.Types.UI.CancellationReasonLookup
import qualified Control.Lens
import qualified Domain.Action.UI.CancellationReasonLookup
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "ride" :> Capture "rideId" (Kernel.Types.Id.Id Domain.Types.Ride.Ride) :> "getCancellationReasons"
      :> Get
           ('[JSON])
           [API.Types.UI.CancellationReasonLookup.CancellationReasonResp]
  )

handler :: Environment.FlowServer API
handler = getRideGetCancellationReasons

getRideGetCancellationReasons ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    Environment.FlowHandler [API.Types.UI.CancellationReasonLookup.CancellationReasonResp]
  )
getRideGetCancellationReasons a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.CancellationReasonLookup.getRideGetCancellationReasons (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
