module Product.APIMapper where

import App.Types (FlowHandler)
import Beckn.Types.FMD.API.Cancel (CancelReq, CancelRes)
import Beckn.Types.FMD.API.Confirm (ConfirmReq, ConfirmRes)
import Beckn.Types.FMD.API.Init (InitReq, InitRes)
import Beckn.Types.FMD.API.Search (SearchReq, SearchRes)
import Beckn.Types.FMD.API.Select (SelectReq, SelectRes)
import Beckn.Types.FMD.API.Status (StatusReq, StatusRes)
import Beckn.Types.FMD.API.Track (TrackReq, TrackRes)
import EulerHS.Prelude

search :: () -> SearchReq -> FlowHandler SearchRes
search _ req = error "Not implemented yet"

select :: () -> SelectReq -> FlowHandler SelectRes
select _ req = error "Not implemented yet"

init :: () -> InitReq -> FlowHandler InitRes
init _ req = error "Not implemented yet"

confirm :: () -> ConfirmReq -> FlowHandler ConfirmRes
confirm _ req = error "Not implemented yet"

track :: () -> TrackReq -> FlowHandler TrackRes
track _ req = error "Not implemented yet"

status :: () -> StatusReq -> FlowHandler StatusRes
status _ req = error "Not implemented yet"

cancel :: () -> CancelReq -> FlowHandler CancelRes
cancel _ req = error "Not implemented yet"
