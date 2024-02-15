{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.SDKEvents where

import API.Types.UI.SDKEvents
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Environment
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Servant
import Tools.Auth
import Tools.Event

postSdkEvents :: (Maybe (Id DP.Person), Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> SDKEventsReq -> Flow APISuccess
postSdkEvents (personId, merchantId, merchantOperatingCityId) SDKEventsReq {..} = do
  triggerSDKEvent $ SDKEventData personId merchantId merchantOperatingCityId event
  return Success
