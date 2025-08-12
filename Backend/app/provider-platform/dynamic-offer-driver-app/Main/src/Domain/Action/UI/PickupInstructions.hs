{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.PickupInstructions (getDriverPickupInstructions) where

import qualified API.Types.UI.PickupInstructions
import qualified Data.HashMap.Strict as HM
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import Kernel.Prelude
import Kernel.Types.Common (BaseUrl)
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Logging (logInfo)
import SharedLogic.CallBAPInternal (AppBackendBapInternal)
import qualified SharedLogic.CallBAPInternal as CallBAPInternal
import Tools.Error
import Tools.Metrics

getDriverPickupInstructions ::
  ( HasFlowEnv m r '["appBackendBapInternal" ::: AppBackendBapInternal, "internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    MonadFlow m,
    CoreMetrics m
  ) =>
  ( ( Maybe (Id Domain.Types.Person.Person),
      Id Domain.Types.Merchant.Merchant,
      Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Text ->
    m API.Types.UI.PickupInstructions.PickupInstructionResp
  )
getDriverPickupInstructions (_, _, _) riderId = do
  logInfo $ "GetPickupInstructions: Fetching pickup instructions for riderId: " <> riderId

  -- Get BAP internal API configuration from environment
  appBackendBapInternal <- asks (.appBackendBapInternal)
  logInfo $ "GetPickupInstructions: Using BAP internal API: " <> appBackendBapInternal.name

  -- Call BAP internal API to get pickup instructions
  result <-
    CallBAPInternal.getPickupInstructions
      appBackendBapInternal.apiKey
      appBackendBapInternal.url
      riderId

  logInfo $ "GetPickupInstructions: Successfully retrieved pickup instructions for riderId: " <> riderId
  pure result
