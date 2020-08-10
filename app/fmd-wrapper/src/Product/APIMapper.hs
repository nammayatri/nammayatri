module Product.APIMapper where

import App.Types (FlowHandler)
import Beckn.Types.FMD.API.Cancel (CancelReq, CancelRes)
import Beckn.Types.FMD.API.Confirm (ConfirmReq, ConfirmRes)
import Beckn.Types.FMD.API.Init (InitReq, InitRes)
import Beckn.Types.FMD.API.Search (SearchReq, SearchRes)
import Beckn.Types.FMD.API.Select (SelectReq, SelectRes)
import Beckn.Types.FMD.API.Status (StatusReq, StatusRes)
import Beckn.Types.FMD.API.Track (TrackReq, TrackRes)
import Beckn.Types.FMD.API.Update (UpdateReq, UpdateRes)
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common (withFlowHandler)
import EulerHS.Prelude
import qualified Product.Dunzo.Flow as DZ

-- TODO: add switching logic to figure out the client instance
search :: Organization -> SearchReq -> FlowHandler SearchRes
search org req = withFlowHandler $ DZ.search org req

select :: Organization -> SelectReq -> FlowHandler SelectRes
select org req = withFlowHandler $ DZ.select org req

init :: Organization -> InitReq -> FlowHandler InitRes
init org req = withFlowHandler $ DZ.init org req

confirm :: Organization -> ConfirmReq -> FlowHandler ConfirmRes
confirm org req = withFlowHandler $ DZ.confirm org req

track :: Organization -> TrackReq -> FlowHandler TrackRes
track org req = withFlowHandler $ DZ.track org req

status :: Organization -> StatusReq -> FlowHandler StatusRes
status org req = withFlowHandler $ DZ.status org req

cancel :: Organization -> CancelReq -> FlowHandler CancelRes
cancel org req = withFlowHandler $ DZ.cancel org req

update :: Organization -> UpdateReq -> FlowHandler UpdateRes
update org req = withFlowHandler $ DZ.update org req
