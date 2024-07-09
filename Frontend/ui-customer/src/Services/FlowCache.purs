{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Services.FlowCache where

import Prelude
import Services.API
import Control.Monad.Except.Trans
import Services.Backend as Remote
import Engineering.Helpers.BackTrack
import Types.App
import Data.Maybe
import ModifyScreenState
import Screens.Types
import Data.Either (Either(..))
import Presto.Core.Flow
import Engineering.Helpers.Commons
import Effect.Class
import Data.Array(filter,null,any)
import Screens.HomeScreen.Transformer (getFareProductType)
import Accessor(_fareProductType)
import Data.Lens ((^.))
import Screens.Types (FareProductType(..)) as FPT
import Engineering.Helpers.Commons as EHC

-------------------------------- GET Saved Location List API--------------------------
updateAndFetchSavedLocations :: Boolean -> FlowBT String SavedLocationsListRes
updateAndFetchSavedLocations needApiCall = do
  (GlobalState state) <- getState
  let
    savedLocations = state.globalFlowCache.savedLocations
  if (not $ isJust savedLocations) || needApiCall then do
    (savedLocationResp) <- lift $ lift $ Remote.getSavedLocationList ""
    case savedLocationResp of
      Right resp -> do
        modifyScreenState $ GlobalFlowCacheType (\globalFlowCache -> globalFlowCache { savedLocations = Just resp })
        pure $ resp
      Left _ -> pure $ dummySavedLocationsListRes
  else
    pure $ fromMaybe dummySavedLocationsListRes savedLocations

updateAndFetchSavedLocationsBT :: SavedLocationReq -> Boolean -> FlowBT String SavedLocationsListRes
updateAndFetchSavedLocationsBT payload needApiCall = do
  (GlobalState state) <- getState
  let
    savedLocations = state.globalFlowCache.savedLocations
  if (not $ isJust savedLocations) || needApiCall then do
    savedLocationsListRes <- Remote.getSavedLocationBT payload
    modifyScreenState $ GlobalFlowCacheType (\globalFlowCache -> globalFlowCache { savedLocations = Just savedLocationsListRes })
    pure $ savedLocationsListRes
  else
    pure $ fromMaybe dummySavedLocationsListRes savedLocations

dummySavedLocationsListRes :: SavedLocationsListRes
dummySavedLocationsListRes =
  SavedLocationsListRes
    { list: []
    }

fetchAndUpdateScheduledRides :: Boolean -> FlowBT String (Maybe RideBookingListRes) 
fetchAndUpdateScheduledRides needApiCall = do
  (GlobalState state) <- getState 
  let 
      savedScheduledRides = state.globalFlowCache.savedScheduledRides
  if (not $ isJust savedScheduledRides) || needApiCall then do 
    fetchResponse <- fetchScheduledRides
    modifyScreenState $ GlobalFlowCacheType (\globalFlowCache -> globalFlowCache { savedScheduledRides = fetchResponse })
    pure $ fetchResponse 
  else 
    pure $ savedScheduledRides 

fetchScheduledRides :: FlowBT String (Maybe RideBookingListRes)
fetchScheduledRides = do
  rideBookingListResponse <- lift $ lift $ Remote.rideBookingListWithStatus "2" "0" "CONFIRMED" Nothing
  pure $ case rideBookingListResponse of
            Right (RideBookingListRes listResp) -> 
                let filteredRides = filter (\(RideBookingRes resp) -> 
                      let fareProductType = getFareProductType $ resp.bookingDetails ^. _fareProductType
                      in fareProductType == FPT.RENTAL || fareProductType == FPT.INTER_CITY) (listResp.list)
                in if not (null filteredRides) 
                    then Just $ RideBookingListRes $ listResp { list = filteredRides } 
                    else Nothing
            Left _ -> Nothing

overlappingRides :: Int -> Int -> FlowBT String Boolean 
overlappingRides maxEstimatedDuration overlappingPollingTime = do 
  (GlobalState state) <- getState
  let 
    savedScheduledRides = state.globalFlowCache.savedScheduledRides
  pure $ case savedScheduledRides of 
      Just (RideBookingListRes response) -> 
        any (\(RideBookingRes resp) -> 
          let
            rideScheduledTime = fromMaybe "" (resp.rideScheduledTime)
            currentBookingEstimatedEnd = EHC.getUTCAfterNSeconds (EHC.getCurrentUTC "") ((maxEstimatedDuration + overlappingPollingTime)* 60 )
            diffInSeconds = EHC.compareUTCDate rideScheduledTime currentBookingEstimatedEnd < 0
          in diffInSeconds) response.list
      Nothing -> false