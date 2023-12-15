{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Dashboard.Tickets where

import qualified API.Types.UI.TicketService as ATB
import Data.Time
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
import SharedLogic.Merchant
import Storage.Beam.SystemConfigs ()

data TicketBookingEndpoint
  = VerifyBookingDetails
  deriving (Show, Read)

derivePersistField "TicketBookingEndpoint"

type VerifyBookingDetailsAPI =
  Capture "personServiceId" (Id DTB.TicketService)
    :> Capture "ticketBookingShortId" (ShortId DTB.TicketBookingService)
    :> "verify"
    :> Post '[JSON] ATB.TicketServiceVerificationResp

type GetServicesAPI =
  Capture "ticketPlaceId" (Id DTB.TicketPlace)
    :> QueryParam "date" Day
    :> "services"
    :> Post '[JSON] [ATB.TicketServiceResp]

type UpdateSeatManagementAPI =
  "update"
    :> ReqBody '[JSON] ATB.TicketBookingUpdateSeatsReq
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

verifyBookingDetails :: ShortId DM.Merchant -> Id DTB.TicketService -> ShortId DTB.TicketBookingService -> FlowHandler ATB.TicketServiceVerificationResp
verifyBookingDetails merchantShortId personServiceId ticketBookingServiceShortId = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantShortId
  withFlowHandlerAPI $ DTB.postTicketBookingsVerify (Nothing, m.id) personServiceId ticketBookingServiceShortId

getServices :: ShortId DM.Merchant -> Id DTB.TicketPlace -> Maybe Day -> FlowHandler [ATB.TicketServiceResp]
getServices merchantShortId ticketPlaceId date = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantShortId
  withFlowHandlerAPI $ DTB.getTicketPlacesServices (Nothing, m.id) ticketPlaceId date

updateSeatManagement :: ShortId DM.Merchant -> ATB.TicketBookingUpdateSeatsReq -> FlowHandler APISuccess
updateSeatManagement merchantShortId req = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantShortId
  withFlowHandlerAPI $ DTB.postTicketBookingsUpdateSeats (Nothing, m.id) req
