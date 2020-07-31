module Product.APIMapper where

import App.Types (FlowHandler)
import Beckn.Types.FMD.API.Cancel (CancelReq, CancelRes)
import Beckn.Types.FMD.API.Confirm (ConfirmReq, ConfirmRes)
import Beckn.Types.FMD.API.Init (InitReq, InitRes)
import Beckn.Types.FMD.API.Search (SearchReq, SearchRes)
import Beckn.Types.FMD.API.Select (SelectReq, SelectRes)
import Beckn.Types.FMD.API.Status (StatusReq, StatusRes)
import Beckn.Types.FMD.API.Track (TrackReq, TrackRes)
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common (withFlowHandler)
import EulerHS.Prelude
import qualified Product.Dunzo.Flow as DZ

-- TODO: add switching logic to figure out the client instance
search :: Organization -> SearchReq -> FlowHandler SearchRes
search org req = withFlowHandler $ DZ.search org req

select :: Organization -> SelectReq -> FlowHandler SelectRes
select org req = error "Not implemented yet"

init :: Organization -> InitReq -> FlowHandler InitRes
init org req = error "Not implemented yet"

confirm :: Organization -> ConfirmReq -> FlowHandler ConfirmRes
confirm org req = error "Not implemented yet"

track :: Organization -> TrackReq -> FlowHandler TrackRes
track org req = error "Not implemented yet"

status :: Organization -> StatusReq -> FlowHandler StatusRes
status org req = error "Not implemented yet"

cancel :: Organization -> CancelReq -> FlowHandler CancelRes
cancel org req = error "Not implemented yet"
