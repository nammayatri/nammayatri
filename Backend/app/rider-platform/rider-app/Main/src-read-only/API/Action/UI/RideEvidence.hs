{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.RideEvidence
  ( API,
    handler,
  )
where

import qualified API.Types.UI.RideEvidence
import qualified Control.Lens
import qualified Domain.Action.UI.RideEvidence
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import qualified Tools.ActorInfo
import Tools.Auth

type API =
  ( TokenAuth :> "ride" :> Capture "rideId" (Kernel.Types.Id.Id Domain.Types.Ride.Ride) :> "evidence"
      :> Get
           ('[JSON])
           [API.Types.UI.RideEvidence.RideEvidenceItem]
      :<|> TokenAuth
      :> "ride"
      :> Capture "rideId" (Kernel.Types.Id.Id Domain.Types.Ride.Ride)
      :> "evidence"
      :> Capture
           "evidenceId"
           Kernel.Prelude.Text
      :> "file"
      :> Get
           ('[JSON])
           Kernel.Prelude.Text
  )

handler :: Environment.FlowServer API
handler = getRideEvidence :<|> getRideEvidenceFile

getRideEvidence ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    Environment.FlowHandler [API.Types.UI.RideEvidence.RideEvidenceItem]
  )
getRideEvidence a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.RideEvidence.getRideEvidence (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getRideEvidenceFile ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    Kernel.Prelude.Text ->
    Environment.FlowHandler Kernel.Prelude.Text
  )
getRideEvidenceFile a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a3) $ Domain.Action.UI.RideEvidence.getRideEvidenceFile (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1
