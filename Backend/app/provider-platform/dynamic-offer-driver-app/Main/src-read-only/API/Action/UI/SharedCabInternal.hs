{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.SharedCabInternal
  ( API,
    handler,
  )
where

import qualified API.Types.UI.SharedCabInternal
import qualified Domain.Action.UI.SharedCabInternal
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import qualified Tools.ActorInfo
import Tools.Auth
import qualified Tools.Notifications

type API =
  ( "sharedCabAllocationFCM" :> Header "token" Kernel.Prelude.Text :> ReqBody '[JSON] Tools.Notifications.SharedCabAllocationEntityData
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> "sharedCabCheckInFCM"
      :> Header "token" Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           API.Types.UI.SharedCabInternal.SharedCabCheckInReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> "sharedCabMoveWarningFCM"
      :> Header
           "token"
           Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           API.Types.UI.SharedCabInternal.SharedCabMoveWarningReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> "sharedCab"
      :> "driver"
      :> Capture
           "driverId"
           Kernel.Prelude.Text
      :> "callNumber"
      :> Header
           "token"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           API.Types.UI.SharedCabInternal.SharedCabDriverCallNumberResp
      :<|> "sharedCab"
      :> Capture
           "merchantId"
           (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant)
      :> "trip"
      :> "active"
      :> MandatoryQueryParam
           "vehicleNumber"
           Kernel.Prelude.Text
      :> Header
           "token"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           (Kernel.Prelude.Maybe API.Types.UI.SharedCabInternal.SharedCabTrip)
      :<|> "sharedCab"
      :> Capture
           "merchantId"
           (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant)
      :> "trip"
      :> Capture
           "tripId"
           Kernel.Prelude.Text
      :> "expire"
      :> QueryParam
           "earnings"
           Kernel.Types.Common.HighPrecMoney
      :> Header
           "token"
           Kernel.Prelude.Text
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = postSharedCabAllocationFCM :<|> postSharedCabCheckInFCM :<|> postSharedCabMoveWarningFCM :<|> getSharedCabDriverCallNumber :<|> getSharedCabTripActive :<|> postSharedCabTripExpire

postSharedCabAllocationFCM :: (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Tools.Notifications.SharedCabAllocationEntityData -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postSharedCabAllocationFCM a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.SharedCabInternal.postSharedCabAllocationFCM a2 a1

postSharedCabCheckInFCM :: (Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.UI.SharedCabInternal.SharedCabCheckInReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postSharedCabCheckInFCM a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.SharedCabInternal.postSharedCabCheckInFCM a2 a1

postSharedCabMoveWarningFCM :: (Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.UI.SharedCabInternal.SharedCabMoveWarningReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postSharedCabMoveWarningFCM a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.SharedCabInternal.postSharedCabMoveWarningFCM a2 a1

getSharedCabDriverCallNumber :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.UI.SharedCabInternal.SharedCabDriverCallNumberResp)
getSharedCabDriverCallNumber a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.SharedCabInternal.getSharedCabDriverCallNumber a2 a1

getSharedCabTripActive :: (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler (Kernel.Prelude.Maybe API.Types.UI.SharedCabInternal.SharedCabTrip))
getSharedCabTripActive a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.SharedCabInternal.getSharedCabTripActive a3 a2 a1

postSharedCabTripExpire :: (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postSharedCabTripExpire a4 a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.SharedCabInternal.postSharedCabTripExpire a4 a3 a2 a1
