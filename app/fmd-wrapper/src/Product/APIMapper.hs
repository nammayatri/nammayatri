{-# LANGUAGE OverloadedLabels #-}

module Product.APIMapper where

import App.Types
import Beckn.Types.Core.Context
import Beckn.Types.Core.Domain
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
  validateRequest $ req ^. #context
  DZ.search org req

select :: Organization -> SelectReq -> FlowHandler SelectRes
select org req = withFlowHandler $ do
  validateRequest $ req ^. #context
  DZ.select org req

init :: Organization -> InitReq -> FlowHandler InitRes
init org req = withFlowHandler $ do
  validateRequest $ req ^. #context
  DZ.init org req

confirm :: Organization -> ConfirmReq -> FlowHandler ConfirmRes
confirm org req = withFlowHandler $ do
  validateRequest $ req ^. #context
  DZ.confirm org req

track :: Organization -> TrackReq -> FlowHandler TrackRes
track org req = withFlowHandler $ do
  validateRequest $ req ^. #context
  DZ.track org req

status :: Organization -> StatusReq -> FlowHandler StatusRes
status org req = withFlowHandler $ do
  validateRequest $ req ^. #context
  DZ.status org req

cancel :: Organization -> CancelReq -> FlowHandler CancelRes
cancel org req = withFlowHandler $ do
  validateRequest $ req ^. #context
  DZ.cancel org req

update :: Organization -> UpdateReq -> FlowHandler UpdateRes
update org req = withFlowHandler $ do
  validateRequest $ req ^. #context
  DZ.update org req

validateDomain :: Context -> Bool
validateDomain context = context ^. #_domain == FINAL_MILE_DELIVERY

-- TODO: get the supported core version from the config file
validateCoreVersion :: Context -> Bool
validateCoreVersion context = maybe False (== "0.8.0") $ context ^. #_core_version

-- TODO: get the supported domain version from the config file
validateDomainVersion :: Context -> Bool
validateDomainVersion context = maybe False (== "0.7.0") $ context ^. #_domain_version

validateRequest :: Context -> Flow ()
validateRequest context = do
  unless (validateDomain context) $
    throwJsonError400 "ApiMapper.validateRequest" "INVALID_DOMAIN"
  unless (validateCoreVersion context) $
    throwJsonError400 "ApiMapper.validateRequest" "UNSUPPORTED_CORE_VERSION"
  unless (validateDomainVersion context) $
    throwJsonError400 "ApiMapper.validateRequest" "UNSUPPORTED_DOMAIN_VERSION"
