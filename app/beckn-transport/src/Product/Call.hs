{-# LANGUAGE OverloadedLabels #-}

module Product.Call where

import App.Types
import Beckn.Types.Core.API.Call
import Beckn.Types.Id
import qualified Beckn.Types.Storage.RegistrationToken as SR
import EulerHS.Prelude
import External.Gateway.Flow as Gateway
import qualified Storage.Queries.ProductInstance as PI
import Types.Error
import Utils.Common

initiateCall :: SR.RegistrationToken -> CallReq -> FlowHandler CallRes
initiateCall _ req = withFlowHandlerAPI $ do
  prdInstance <- PI.findById $ Id $ req ^. #productInstanceId -- RIDEORDER PI
  Id rideSearchProductInstanceId <- prdInstance ^. #_parentId & fromMaybeM (PIFieldNotPresent "parent_id")
  Gateway.initiateCall $ CallReq rideSearchProductInstanceId -- RIDESEARCH PI
