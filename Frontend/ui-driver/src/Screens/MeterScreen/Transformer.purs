module Screens.MeterScreen.Transformer where

import Helpers.Utils (fetchImage, FetchImageFrom(..), parseFloat)
import Data.Maybe (Maybe(..), fromMaybe)
import Screens.Types (LocItemType(..), LocationItemType(..), LocationListItemState(..))
import Data.Int (toNumber)
import Prelude (map, (>=), (>), (+), (&&), ($), (<>), show, (<=), (/), otherwise)
import Services.API (Prediction(..))
import Data.Array as DA
import Data.Lens ((^.))
import Services.Accessor (_description, _place_id, _types, _distance)
import Data.String (Pattern(..), split, drop, indexOf)
import Screens.MeterScreen.ScreenData (dummyAddress)

getLocationList :: Array Prediction -> Array LocationListItemState
getLocationList prediction = map (\x -> getLocation x) prediction

getLocation :: Prediction -> LocationListItemState
getLocation prediction = {
    prefixImageUrl : fetchImage FF_ASSET "ny_ic_loc_grey"
  , postfixImageUrl : fetchImage FF_ASSET "ny_ic_fav"
  , postfixImageVisibility : true
  , title : (fromMaybe "" ((split (Pattern ",") (prediction ^. _description)) DA.!! 0))
  , subTitle : (drop ((fromMaybe 0 (indexOf (Pattern ",") (prediction ^. _description))) + 2) (prediction ^. _description))
  , placeId : prediction ^._place_id
  , lat : Nothing
  , lon : Nothing
  , description : prediction ^. _description
  , tag : ""
  , tagType : Just $ show LOC_LIST
  , cardType : Nothing
  , address : ""
  , tagName : ""
  , isEditEnabled : true
  , savedLocation : ""
  , placeName : ""
  , isClickable : true
  , alpha : 1.0
  , fullAddress : dummyAddress
  , locationItemType : Just PREDICTION
  , distance : Just (fromMetersToKm (fromMaybe 0 (prediction ^._distance)))
  , showDistance : Just $ checkShowDistance (fromMaybe 0 (prediction ^._distance))
  , actualDistance : (prediction ^._distance)
  , frequencyCount : Nothing
  , recencyDate : Nothing
  , locationScore : Nothing
  , dynamicAction : Nothing
  , types : Nothing
}

checkShowDistance :: Int ->  Boolean
checkShowDistance distance = (distance > 0 && distance <= 50000)

fromMetersToKm :: Int -> String
fromMetersToKm distanceInMeters
  | distanceInMeters >= 1000 = parseFloat (toNumber distanceInMeters / 1000.0) 1 <> " km"
  | otherwise = show distanceInMeters <> " m"
