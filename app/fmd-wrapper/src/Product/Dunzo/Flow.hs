module Product.Dunzo.Flow where

import App.Types
import Beckn.Types.FMD.API.Cancel (CancelReq, CancelRes)
import Beckn.Types.FMD.API.Confirm (ConfirmReq, ConfirmRes)
import Beckn.Types.FMD.API.Init (InitReq, InitRes)
import Beckn.Types.FMD.API.Search (SearchReq, SearchRes)
import Beckn.Types.FMD.API.Select (SelectReq, SelectRes)
import Beckn.Types.FMD.API.Status (StatusReq, StatusRes)
import Beckn.Types.FMD.API.Track (TrackReq, TrackRes)
import Beckn.Types.Storage.Organization (Organization)
import EulerHS.Prelude

search :: Organization -> SearchReq -> Flow SearchRes
search org req = error "Not implemented yet"

select :: Organization -> SelectReq -> Flow SelectRes
select org req = error "Not implemented yet"

init :: Organization -> InitReq -> Flow InitRes
init org req = error "Not implemented yet"

confirm :: Organization -> ConfirmReq -> Flow ConfirmRes
confirm org req = error "Not implemented yet"

track :: Organization -> TrackReq -> Flow TrackRes
track org req = error "Not implemented yet"

status :: Organization -> StatusReq -> Flow StatusRes
status org req = error "Not implemented yet"

cancel :: Organization -> CancelReq -> Flow CancelRes
cancel org req = error "Not implemented yet"
