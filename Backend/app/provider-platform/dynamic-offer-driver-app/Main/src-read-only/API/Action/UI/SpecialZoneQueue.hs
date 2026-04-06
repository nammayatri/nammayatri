{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.SpecialZoneQueue
  ( API,
    handler,
  )
where

import qualified API.Types.UI.SpecialZoneQueue
import qualified Control.Lens
import qualified Domain.Action.UI.SpecialZoneQueue
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.SpecialZoneQueueRequest
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
  ( TokenAuth :> "specialZoneQueue" :> "request"
      :> Get
           ('[JSON])
           API.Types.UI.SpecialZoneQueue.SpecialZoneQueueRequestListRes
      :<|> TokenAuth
      :> "specialZoneQueue"
      :> "request"
      :> Capture
           "requestId"
           (Kernel.Types.Id.Id Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest)
      :> "respond"
      :> ReqBody
           ('[JSON])
           API.Types.UI.SpecialZoneQueue.SpecialZoneQueueRespondReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "specialZoneQueue"
      :> "request"
      :> Capture
           "requestId"
           (Kernel.Types.Id.Id Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest)
      :> "cancel"
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = getSpecialZoneQueueRequest :<|> postSpecialZoneQueueRequestRespond :<|> postSpecialZoneQueueRequestCancel

getSpecialZoneQueueRequest ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler API.Types.UI.SpecialZoneQueue.SpecialZoneQueueRequestListRes
  )
getSpecialZoneQueueRequest a1 = withFlowHandlerAPI $ Domain.Action.UI.SpecialZoneQueue.getSpecialZoneQueueRequest (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

postSpecialZoneQueueRequestRespond ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest ->
    API.Types.UI.SpecialZoneQueue.SpecialZoneQueueRespondReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postSpecialZoneQueueRequestRespond a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SpecialZoneQueue.postSpecialZoneQueueRequestRespond (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postSpecialZoneQueueRequestCancel ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postSpecialZoneQueueRequestCancel a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SpecialZoneQueue.postSpecialZoneQueueRequestCancel (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
