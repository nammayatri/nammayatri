{-# LANGUAGE OverloadedLabels #-}

module Product.Call where

import App.Types
import Beckn.Types.Core.API.Call
import Beckn.Types.ID
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import EulerHS.Prelude
import External.Gateway.Flow as Gateway
import qualified Storage.Queries.ProductInstance as PI

initiateCall :: SR.RegistrationToken -> CallReq -> FlowHandler CallRes
initiateCall _ req = withFlowHandler $ do
  prdInstance <- PI.findById $ ID $ req ^. #productInstanceId -- RIDEORDER PI
  ID rideSearchProductInstanceId <- prdInstance ^. #_parentId & fromMaybeM500 "ParentId not found"
  Gateway.initiateCall $ CallReq rideSearchProductInstanceId -- RIDESEARCH PI
