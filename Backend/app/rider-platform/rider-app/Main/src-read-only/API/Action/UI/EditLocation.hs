{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.EditLocation
  ( API,
    handler,
  )
where

import qualified API.Types.UI.EditLocation
import qualified Control.Lens
import qualified Domain.Action.UI.EditLocation
import qualified Domain.Types.BookingUpdateRequest
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
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
  ( TokenAuth :> "edit" :> Capture "bookingUpdateRequestId" (Kernel.Types.Id.Id Domain.Types.BookingUpdateRequest.BookingUpdateRequest) :> "result"
      :> Get
           '[JSON]
           API.Types.UI.EditLocation.EditLocationResultAPIResp
      :<|> TokenAuth
      :> "edit"
      :> "result"
      :> Capture
           "bookingUpdateRequestId"
           (Kernel.Types.Id.Id Domain.Types.BookingUpdateRequest.BookingUpdateRequest)
      :> "confirm"
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = getEditResult :<|> postEditResultConfirm

getEditResult ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.BookingUpdateRequest.BookingUpdateRequest ->
    Environment.FlowHandler API.Types.UI.EditLocation.EditLocationResultAPIResp
  )
getEditResult a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.EditLocation.getEditResult (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postEditResultConfirm ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.BookingUpdateRequest.BookingUpdateRequest ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postEditResultConfirm a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.EditLocation.postEditResultConfirm (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
