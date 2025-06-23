{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.AppManagement.Tickets
  ( API.Types.Dashboard.AppManagement.Tickets.API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.Tickets
import qualified "this" API.Types.UI.TicketService
import qualified Data.Time.Calendar
import qualified Domain.Action.Dashboard.AppManagement.Tickets as Domain.Action.Dashboard.AppManagement.Tickets
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.MerchantOnboarding
import qualified "this" Domain.Types.TicketBooking
import qualified "this" Domain.Types.TicketBookingService
import qualified "this" Domain.Types.TicketDashboard
import qualified "this" Domain.Types.TicketPlace
import qualified "this" Domain.Types.TicketService
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.AppManagement.Tickets.API)
handler merchantId city = postTicketsVerify merchantId city :<|> postTicketsServices merchantId city :<|> getTicketsPlaces merchantId city :<|> postTicketsUpdate merchantId city :<|> postTicketsBookingsCancel merchantId city :<|> postTicketsServiceCancel merchantId city :<|> getTicketsBookingDetails merchantId city :<|> postTicketsTicketdashboardRegister merchantId city :<|> postTicketsTicketdashboardLoginAuth merchantId city :<|> postTicketsTicketdashboardLoginVerify merchantId city :<|> getTicketsTicketdashboardAgreement merchantId city :<|> getTicketsTicketdashboardUserInfo merchantId city :<|> getTicketsTicketdashboardFile merchantId city :<|> postTicketsTicketdashboardSendverifyotp merchantId city :<|> getTicketsTicketdashboardTicketplaceInfo merchantId city :<|> postTicketsTicketdashboardTicketplaceUpdate merchantId city :<|> getTicketsTicketdashboardTicketplaces merchantId city

postTicketsVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.TicketService.TicketService -> Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService -> Environment.FlowHandler API.Types.UI.TicketService.TicketServiceVerificationResp)
postTicketsVerify a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.postTicketsVerify a4 a3 a2 a1

postTicketsServices :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe Data.Time.Calendar.Day -> Environment.FlowHandler [API.Types.UI.TicketService.TicketServiceResp])
postTicketsServices a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.postTicketsServices a4 a3 a2 a1

getTicketsPlaces :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler [Domain.Types.TicketPlace.TicketPlace])
getTicketsPlaces a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.getTicketsPlaces a2 a1

postTicketsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.UI.TicketService.TicketBookingUpdateSeatsReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postTicketsUpdate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.postTicketsUpdate a3 a2 a1

postTicketsBookingsCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.UI.TicketService.TicketBookingCancelReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postTicketsBookingsCancel a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.postTicketsBookingsCancel a3 a2 a1

postTicketsServiceCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.UI.TicketService.TicketServiceCancelReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postTicketsServiceCancel a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.postTicketsServiceCancel a3 a2 a1

getTicketsBookingDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking -> Environment.FlowHandler API.Types.UI.TicketService.TicketBookingDetails)
getTicketsBookingDetails a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.getTicketsBookingDetails a3 a2 a1

postTicketsTicketdashboardRegister :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.Tickets.TicketDashboardRegisterReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Tickets.TicketDashboardRegisterResp)
postTicketsTicketdashboardRegister a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.postTicketsTicketdashboardRegister a3 a2 a1

postTicketsTicketdashboardLoginAuth :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.Tickets.TicketDashboardLoginReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postTicketsTicketdashboardLoginAuth a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.postTicketsTicketdashboardLoginAuth a3 a2 a1

postTicketsTicketdashboardLoginVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.Tickets.TicketDashboardLoginReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Tickets.TicketDashboardLoginResp)
postTicketsTicketdashboardLoginVerify a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.postTicketsTicketdashboardLoginVerify a3 a2 a1

getTicketsTicketdashboardAgreement :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Tickets.TicketDashboardAgreementTemplateResp)
getTicketsTicketdashboardAgreement a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.getTicketsTicketdashboardAgreement a3 a2 a1

getTicketsTicketdashboardUserInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Tickets.TicketDashboardUserInfo)
getTicketsTicketdashboardUserInfo a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.getTicketsTicketdashboardUserInfo a5 a4 a3 a2 a1

getTicketsTicketdashboardFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Environment.FlowHandler Domain.Types.MerchantOnboarding.GetFileResponse)
getTicketsTicketdashboardFile a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.getTicketsTicketdashboardFile a5 a4 a3 a2 a1

postTicketsTicketdashboardSendverifyotp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.Tickets.SendVerifyOtpReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postTicketsTicketdashboardSendverifyotp a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.postTicketsTicketdashboardSendverifyotp a3 a2 a1

getTicketsTicketdashboardTicketplaceInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Environment.FlowHandler Domain.Types.TicketDashboard.TicketPlaceDashboardDetails)
getTicketsTicketdashboardTicketplaceInfo a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.getTicketsTicketdashboardTicketplaceInfo a5 a4 a3 a2 a1

postTicketsTicketdashboardTicketplaceUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Domain.Types.TicketDashboard.TicketPlaceDashboardDetails -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postTicketsTicketdashboardTicketplaceUpdate a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.postTicketsTicketdashboardTicketplaceUpdate a5 a4 a3 a2 a1

getTicketsTicketdashboardTicketplaces :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.MerchantOnboarding.RequestorRole -> Environment.FlowHandler [Domain.Types.TicketPlace.TicketPlace])
getTicketsTicketdashboardTicketplaces a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Tickets.getTicketsTicketdashboardTicketplaces a5 a4 a3 a2 a1
