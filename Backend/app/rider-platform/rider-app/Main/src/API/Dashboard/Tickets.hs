{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Dashboard.Tickets where

import qualified Domain.Action.UI.TicketService as DTB
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.TicketBookingService as DTB
import qualified Domain.Types.TicketPlace as DTB
import qualified Domain.Types.TicketService as DTB
import Environment
import Kernel.Prelude
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)

data TicketBookingEndpoint
  = VerifyBookingDetails
  deriving (Show, Read)

derivePersistField "TicketBookingEndpoint"

type VerifyBookingDetailsAPI =
  Capture "personServiceId" (Id DTB.TicketService)
    :> Capture "ticketBookingShortId" (ShortId DTB.TicketBookingService)
    :> "verify"
    :> Post '[JSON] DTB.TicketServiceVerificationResp

type GetServicesAPI =
  Capture "ticketPlaceId" (Id DTB.TicketPlace)
    :> "services"
    :> Post '[JSON] [DTB.TicketServiceResp]

type UpdateSeatManagementAPI =
  "update"
    :> ReqBody '[JSON] DTB.TicketBookingUpdateSeatsReq
    :> Post '[JSON] APISuccess

type API =
  "tickets"
    :> VerifyBookingDetailsAPI
    :<|> GetServicesAPI
    :<|> UpdateSeatManagementAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  verifyBookingDetails merchantId
    :<|> getServices merchantId
    :<|> updateSeatManagement merchantId

verifyBookingDetails :: ShortId DM.Merchant -> Id DTB.TicketService -> ShortId DTB.TicketBookingService -> FlowHandler DTB.TicketServiceVerificationResp
verifyBookingDetails _ personServiceId = withFlowHandlerAPI . DTB.postTicketBookingsVerify personServiceId

getServices :: ShortId DM.Merchant -> Id DTB.TicketPlace -> FlowHandler [DTB.TicketServiceResp]
getServices _ ticketPlaceId = withFlowHandlerAPI $ DTB.getTicketPlacesServices ticketPlaceId

updateSeatManagement :: ShortId DM.Merchant -> DTB.TicketBookingUpdateSeatsReq -> FlowHandler APISuccess
updateSeatManagement _ = withFlowHandlerAPI . DTB.postTicketBookingsUpdateSeats
