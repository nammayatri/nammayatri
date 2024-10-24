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
import Data.Array as DA
import Screens.HomeScreen.Transformer (getFareProductType)
import Accessor(_fareProductType)
import Data.Lens ((^.))
import Screens.Types (FareProductType(..)) as FPT
import Engineering.Helpers.Commons as EHC
import Helpers.Utils(checkOverLap)
import Engineering.Helpers.Commons(getCurrentUTC)
import Common.Types.App as CTA
import Engineering.Helpers.Utils as EHU
import Locale.Utils

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
  rideBookingListResponse <- lift $ lift $ Remote.rideBookingList "5" "0" "true"
  pure $ case rideBookingListResponse of
            Right (RideBookingListRes listResp) -> 
                let 
                  filteredRides = DA.filter (\(RideBookingRes item) -> 
                                            let 
                                                rideScheduledTime = fromMaybe (getCurrentUTC "") item.rideScheduledTime
                                                isScheduled = item.isScheduled || rideScheduledTime > (getCurrentUTC "")
                                            in 
                                            (DA.any (_ == item.status) ["CONFIRMED" , "TRIP_ASSIGNED"]) && isScheduled) (listResp.list)
                in if not (DA.null filteredRides) 
                    then Just $ RideBookingListRes $ listResp { list = filteredRides } 
                    else Nothing
            Left _ -> Nothing

overlappingRides :: String -> Maybe String -> Int -> FlowBT String { overLapping :: Boolean , overLappedBooking :: Maybe RideBookingRes}
overlappingRides rideStartTime maybeRideEndTime overlappingPollingTime = do 
  (GlobalState state) <- getState
  let 
    savedScheduledRides = state.globalFlowCache.savedScheduledRides
    rideEndTime = fromMaybe rideStartTime maybeRideEndTime
  pure $ case savedScheduledRides of 
      Just (RideBookingListRes response) -> do
        let 
          overLappingRide = (DA.find (\item -> isJust (checkOverLap rideStartTime rideEndTime item overlappingPollingTime)) response.list)
        case overLappingRide of 
             Just resp -> { overLapping : true, overLappedBooking : Just resp }
             Nothing   -> { overLapping : false, overLappedBooking : Nothing }
      Nothing -> {overLapping : false , overLappedBooking : Nothing}
  
-------------------------------- GET Issue Category List API--------------------------
fetchIssueCategories :: FlowBT String (Array CTA.CategoryListType)
fetchIssueCategories = do
  let language = EHU.fetchLanguage $ getLanguageLocale languageKey
  (GlobalState state) <- getState
  case state.globalFlowCache.issueCategories of
    Just issueCategoryList -> pure issueCategoryList
    Nothing -> do
      GetCategoriesRes response <- Remote.getCategoriesBT language
      let issueCategoryList =
            map
              (\(Category catObj) ->
                { categoryName : catObj.category,
                  categoryId : catObj.issueCategoryId,
                  categoryAction : Just catObj.label,
                  categoryImageUrl : Just catObj.logoUrl,
                  isRideRequired : catObj.isRideRequired,
                  maxAllowedRideAge : catObj.maxAllowedRideAge,
                  categoryType: catObj.categoryType,
                  allowedRideStatuses: catObj.allowedRideStatuses
                }
            ) response.categories
      modifyScreenState $ GlobalFlowCacheType (\globalFlowCache -> globalFlowCache { issueCategories = Just issueCategoryList})
      pure issueCategoryList