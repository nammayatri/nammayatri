{-# LANGUAGE OverloadedLabels #-}

module Product.APIMapper where

import App.Types
import Beckn.Product.Validation.Context
import Beckn.Types.Core.Context
import Beckn.Types.Core.Domain
import Beckn.Types.Error
import Beckn.Types.FMD.API.Cancel (CancelReq, CancelRes)
import Beckn.Types.FMD.API.Confirm (ConfirmReq, ConfirmRes)
import Beckn.Types.FMD.API.Init (InitReq, InitRes)
import Beckn.Types.FMD.API.Search (SearchReq, SearchRes)
import Beckn.Types.FMD.API.Select (SelectReq, SelectRes)
import Beckn.Types.FMD.API.Status (StatusReq, StatusRes)
import Beckn.Types.FMD.API.Track (TrackReq, TrackRes)
import Beckn.Types.FMD.API.Update (UpdateReq, UpdateRes)
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import EulerHS.Prelude
import qualified Product.Dunzo.Flow as DZ

-- TODO: add switching logic to figure out the client instance
search :: Organization -> SearchReq -> FlowHandler SearchRes
search org req = withFlowHandler $ do
  validateContext "search" $ req ^. #context
  DZ.search org req

select :: Organization -> SelectReq -> FlowHandler SelectRes
select org req = withFlowHandler $ do
  validateContext "select" $ req ^. #context
  validateBapUrl org $ req ^. #context
  DZ.select org req

init :: Organization -> InitReq -> FlowHandler InitRes
init org req = withFlowHandler $ do
  validateContext "init" $ req ^. #context
  validateBapUrl org $ req ^. #context
  DZ.init org req

confirm :: Organization -> ConfirmReq -> FlowHandler ConfirmRes
confirm org req = withFlowHandler $ do
  validateContext "confirm" $ req ^. #context
  validateBapUrl org $ req ^. #context
  DZ.confirm org req

track :: Organization -> TrackReq -> FlowHandler TrackRes
track org req = withFlowHandler $ do
  validateContext "track" $ req ^. #context
  validateBapUrl org $ req ^. #context
  DZ.track org req

status :: Organization -> StatusReq -> FlowHandler StatusRes
status org req = withFlowHandler $ do
  validateContext "status" $ req ^. #context
  validateBapUrl org $ req ^. #context
  DZ.status org req

cancel :: Organization -> CancelReq -> FlowHandler CancelRes
cancel org req = withFlowHandler $ do
  validateContext "cancel" $ req ^. #context
  validateBapUrl org $ req ^. #context
  DZ.cancel org req

update :: Organization -> UpdateReq -> FlowHandler UpdateRes
update org req = withFlowHandler $ do
  validateContext "update" $ req ^. #context
  validateBapUrl org $ req ^. #context
  DZ.update org req

validateContext :: Text -> Context -> Flow ()
validateContext action context = do
  validateDomain FINAL_MILE_DELIVERY context
  validateContextCommons action context

validateBapUrl :: Organization -> Context -> Flow ()
validateBapUrl org context = do
  let satisfied = case context ^. #_bap_uri of
        Nothing -> False
        Just bapUrl -> org ^. #_callbackUrl == Just bapUrl
  unless satisfied $
    throwErrorWithInfo400 CommonError "Invalid bap URL."
