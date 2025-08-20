{-
  Copyright 2022-23, Juspay India Pvt Ltd
-}

module API.Internal.FleetBookingInformation where

import qualified Domain.Action.Internal.FleetBookingInformation as Domain
import Environment (FlowHandler, FlowServer)
import Kernel.Prelude
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant

type API =
  "fleet" :> "bookingInformation" :> (CreateBookingInformation :<|> UpdateBookingInformation)

type CreateBookingInformation =
  "create" :> ReqBody '[JSON] Domain.CreateFleetBookingInformationReq :> Post '[JSON] Domain.CreateFleetBookingInformationResp

type UpdateBookingInformation =
  "update" :> ReqBody '[JSON] Domain.UpdateFleetBookingInformationReq :> Post '[JSON] Domain.UpdateFleetBookingInformationResp

handler :: FlowServer API
handler = createBookingInformation :<|> updateBookingInformation

createBookingInformation :: Domain.CreateFleetBookingInformationReq -> FlowHandler Domain.CreateFleetBookingInformationResp
createBookingInformation req = withFlowHandlerAPI $ snd <$> Domain.createBookingInformation req

updateBookingInformation :: Domain.UpdateFleetBookingInformationReq -> FlowHandler Domain.UpdateFleetBookingInformationResp
updateBookingInformation = withFlowHandlerAPI . Domain.updateBookingInformation
