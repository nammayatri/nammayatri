module Domain.Action.UI.PickupInstructions (getPickupInstructions) where

import qualified API.Types.UI.PickupInstructions
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBAPInternal as CallBAPInternal

getPickupInstructions ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    Environment.Flow API.Types.UI.PickupInstructions.PickupInstructionResp
  )
getPickupInstructions (_, _, _) rideId = do
  logInfo $ "GetPickupInstructions: Fetching pickup instructions for rideId: " <> rideId.getId

  -- Get BAP internal API configuration from environment
  appBackendBapInternal <- asks (.appBackendBapInternal)
  logInfo $ "GetPickupInstructions: Using BAP internal API: " <> appBackendBapInternal.name

  -- Call BAP internal API to get pickup instructions
  result <-
    CallBAPInternal.getPickupInstructions
      appBackendBapInternal.apiKey
      appBackendBapInternal.url
      rideId.getId

  logInfo $ "GetPickupInstructions: Successfully retrieved pickup instructions for rideId: " <> rideId.getId
  pure result
