{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Dashboard.Tickets where

import qualified Domain.Action.UI.Tickets as DTB
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Tickets as DTB
import Environment
import Kernel.Prelude
import Kernel.Storage.Esqueleto (derivePersistField)
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

type API =
  "tickets"
    :> VerifyBookingDetailsAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler = verifyBookingDetails

verifyBookingDetails :: ShortId DM.Merchant -> Id DTB.TicketService -> ShortId DTB.TicketBookingService -> FlowHandler DTB.TicketServiceVerificationResp
verifyBookingDetails _ personServiceId = withFlowHandlerAPI . DTB.verifyBookingDetails personServiceId
