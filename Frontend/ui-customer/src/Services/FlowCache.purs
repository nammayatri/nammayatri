{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Services.FlowCache
  ( updateAndFetchSavedLocationsBT, updateAndFetchSavedLocations, updateAndFetchFavouriteDriver , updateAndFetchFavouriteDriverTrips
  )
  where

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

-------------------------------- GET Favourite Driver List API-----------------------------------

updateAndFetchFavouriteDriver :: FlowBT String GetFavouriteDriverListRes
updateAndFetchFavouriteDriver = do
  -- (favouriteDriversResp) <- lift $ lift $ Remote.getFavouriteDriverList 
  -- case favouriteDriversResp of
  --   Right resp -> pure $ resp
  --   Left _ -> pure $ dummyFavouriteDriversListRes

  let favouriteDriversResp = GetFavouriteDriverListRes {
    list : [
       FavouriteDriverList {
        driverName : "Rammoorthy Parashuram",
        driverPhone : "78687687676",
        driverRating : 4.0,
        favCount : 5,
        id : Just "Don't know"
      }, 
       FavouriteDriverList {
        driverName : "Rammoorthy Parashuram",
        driverPhone : "78687687676",
        driverRating : 4.0,
        favCount : 5,
        id : Just "Don't know"
      }
      ] 
    }
  pure $ favouriteDriversResp


-------------------------------- GET Favourite Driver Trips API-----------------------------------

updateAndFetchFavouriteDriverTrips  :: String -> String -> String -> String -> FlowBT String FavouriteDriverTripsResp
updateAndFetchFavouriteDriverTrips limit offset onlyActive driverNo = do
  -- (favouriteDriversResp) <- lift $ lift $ Remote.getFavouriteDriverTrips limit offset onlyActive driverNo
  -- case favouriteDriversResp of
  --   Right resp -> pure $ resp
  --   Left _ -> pure $ dummyFavouriteDriversTripsRes
  
  let favouriteDriversResp = FavouriteDriverTripsResp {
    list : [
       FavouriteDriverRides {
        rideRating : Just 5
      , fromLocation : Just $ LocationAPIEntity {
        street : Just "27th Cross Rd",
        door : Nothing,
        city : Just "Bengaluru",
        state : Just "Karnataka",
        country : Nothing,
        building : Just "Nagarjuna Apartments,15/2",
        areaCode : Just "560102",
        area : Just "Sector 2",
        ward : Just "19th Main",
        placeId : Nothing
      }
      , toLocation : Just $ LocationAPIEntity {
        street : Just "28th Cross Rd",
        door : Nothing,
        city : Just "Nangal",
        state : Just "Punjab",
        country : Nothing,
        building : Just "Nagarjuna Apartments,15/2",
        areaCode : Just "560102",
        area : Just "Sector 2",
        ward : Just "19th Main",
        placeId : Nothing
      }
      , totalFare : Just 220
      , startTime : Just "String"
      , id : Just ""
      }, 
       FavouriteDriverRides {
        rideRating : Just 5
      , fromLocation : Just $ LocationAPIEntity {
        street : Just "27th Cross Rd",
        door : Nothing,
        city : Just "Bengaluru",
        state : Just "Karnataka",
        country : Nothing,
        building : Just "Nagarjuna Apartments,15/2",
        areaCode : Just "560102",
        area : Just "Sector 2",
        ward : Just "19th Main",
        placeId : Nothing
      }
      , toLocation : Just $ LocationAPIEntity {
        street : Just "28th Cross Rd",
        door : Nothing,
        city : Just "Nangal",
        state : Just "Punjab",
        country : Nothing,
        building : Just "Nagarjuna Apartments,15/2",
        areaCode : Just "560102",
        area : Just "Sector 2",
        ward : Just "19th Main",
        placeId : Nothing
      }
      , totalFare : Just 220
      , startTime : Just "String"
      , id : Just ""
      }, 
       FavouriteDriverRides {
        rideRating : Just 5
      , fromLocation : Just $ LocationAPIEntity {
        street : Just "27th Cross Rd",
        door : Nothing,
        city : Just "Bengaluru",
        state : Just "Karnataka",
        country : Nothing,
        building : Just "Nagarjuna Apartments,15/2",
        areaCode : Just "560102",
        area : Just "Sector 2",
        ward : Just "19th Main",
        placeId : Nothing
      }
      , toLocation : Just $ LocationAPIEntity {
        street : Just "28th Cross Rd",
        door : Nothing,
        city : Just "Nangal",
        state : Just "Punjab",
        country : Nothing,
        building : Just "Nagarjuna Apartments,15/2",
        areaCode : Just "560102",
        area : Just "Sector 2",
        ward : Just "19th Main",
        placeId : Nothing
      }
      , totalFare : Just 220
      , startTime : Just "String"
      , id : Just ""
      }, 
       FavouriteDriverRides {
        rideRating : Just 5
      , fromLocation : Just $ LocationAPIEntity {
        street : Just "27th Cross Rd",
        door : Nothing,
        city : Just "Bengaluru",
        state : Just "Karnataka",
        country : Nothing,
        building : Just "Nagarjuna Apartments,15/2",
        areaCode : Just "560102",
        area : Just "Sector 2",
        ward : Just "19th Main",
        placeId : Nothing
      }
      , toLocation : Just $ LocationAPIEntity {
        street : Just "28th Cross Rd",
        door : Nothing,
        city : Just "Nangal",
        state : Just "Punjab",
        country : Nothing,
        building : Just "Nagarjuna Apartments,15/2",
        areaCode : Just "560102",
        area : Just "Sector 2",
        ward : Just "19th Main",
        placeId : Nothing
      }
      , totalFare : Just 220
      , startTime : Just "String"
      , id : Just ""
      }, 
       FavouriteDriverRides {
        rideRating : Just 5
      , fromLocation : Just $ LocationAPIEntity {
        street : Just "27th Cross Rd",
        door : Nothing,
        city : Just "Bengaluru",
        state : Just "Karnataka",
        country : Nothing,
        building : Just "Nagarjuna Apartments,15/2",
        areaCode : Just "560102",
        area : Just "Sector 2",
        ward : Just "19th Main",
        placeId : Nothing
      }
      , toLocation : Just $ LocationAPIEntity {
        street : Just "28th Cross Rd",
        door : Nothing,
        city : Just "Nangal",
        state : Just "Punjab",
        country : Nothing,
        building : Just "Nagarjuna Apartments,15/2",
        areaCode : Just "560102",
        area : Just "Sector 2",
        ward : Just "19th Main",
        placeId : Nothing
      }
      , totalFare : Just 220
      , startTime : Just "String"
      , id : Just ""
      }
      ] 
    }
  pure $ favouriteDriversResp

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

dummyFavouriteDriversListRes :: GetFavouriteDriverListRes
dummyFavouriteDriversListRes =
  GetFavouriteDriverListRes
    { list: []
    }

dummyFavouriteDriversTripsRes :: FavouriteDriverTripsResp
dummyFavouriteDriversTripsRes =
  FavouriteDriverTripsResp
    { list: []
    }

