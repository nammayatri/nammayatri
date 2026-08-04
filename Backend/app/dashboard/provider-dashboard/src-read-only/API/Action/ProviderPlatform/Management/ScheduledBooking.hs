{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.ScheduledBooking
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.ScheduledBooking
import qualified Domain.Action.ProviderPlatform.Management.ScheduledBooking
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("scheduledBooking" :> (GetScheduledBookingList :<|> GetScheduledBookingInfo :<|> PostScheduledBookingOpsNoteAdd :<|> PutScheduledBookingOpsNoteUpdate))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getScheduledBookingList merchantId city :<|> getScheduledBookingInfo merchantId city :<|> postScheduledBookingOpsNoteAdd merchantId city :<|> putScheduledBookingOpsNoteUpdate merchantId city

type GetScheduledBookingList =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.SCHEDULED_BOOKING) / ('API.Types.ProviderPlatform.Management.ScheduledBooking.GET_SCHEDULED_BOOKING_LIST))
      :> API.Types.ProviderPlatform.Management.ScheduledBooking.GetScheduledBookingList
  )

type GetScheduledBookingInfo =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.SCHEDULED_BOOKING) / ('API.Types.ProviderPlatform.Management.ScheduledBooking.GET_SCHEDULED_BOOKING_INFO))
      :> API.Types.ProviderPlatform.Management.ScheduledBooking.GetScheduledBookingInfo
  )

type PostScheduledBookingOpsNoteAdd =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.SCHEDULED_BOOKING) / ('API.Types.ProviderPlatform.Management.ScheduledBooking.POST_SCHEDULED_BOOKING_OPS_NOTE_ADD))
      :> API.Types.ProviderPlatform.Management.ScheduledBooking.PostScheduledBookingOpsNoteAdd
  )

type PutScheduledBookingOpsNoteUpdate =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.SCHEDULED_BOOKING) / ('API.Types.ProviderPlatform.Management.ScheduledBooking.PUT_SCHEDULED_BOOKING_OPS_NOTE_UPDATE))
      :> API.Types.ProviderPlatform.Management.ScheduledBooking.PutScheduledBookingOpsNoteUpdate
  )

getScheduledBookingList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.ScheduledBooking.AssignmentStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.ScheduledBooking.ScheduledBookingListRes)
getScheduledBookingList merchantShortId opCity apiTokenInfo assignmentStatus from limit offset to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.ScheduledBooking.getScheduledBookingList merchantShortId opCity apiTokenInfo assignmentStatus from limit offset to

getScheduledBookingInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.ScheduledBooking.ScheduledBookingInfoRes)
getScheduledBookingInfo merchantShortId opCity apiTokenInfo transactionId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.ScheduledBooking.getScheduledBookingInfo merchantShortId opCity apiTokenInfo transactionId

postScheduledBookingOpsNoteAdd :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.ScheduledBooking.AddOpsNoteReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postScheduledBookingOpsNoteAdd merchantShortId opCity apiTokenInfo transactionId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.ScheduledBooking.postScheduledBookingOpsNoteAdd merchantShortId opCity apiTokenInfo transactionId req

putScheduledBookingOpsNoteUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.ScheduledBooking.UpdateOpsNoteReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putScheduledBookingOpsNoteUpdate merchantShortId opCity apiTokenInfo noteId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.ScheduledBooking.putScheduledBookingOpsNoteUpdate merchantShortId opCity apiTokenInfo noteId req
