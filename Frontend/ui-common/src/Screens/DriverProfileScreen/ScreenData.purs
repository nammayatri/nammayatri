module Screens.DriverProfileScreenCommon.ScreenData where

import Prelude
import Data.Maybe (Maybe)
import Data.Maybe (Maybe(..))
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Presto.Core.Types.API (class RestEndpoint, class StandardEncode, ErrorPayload, Method(..), defaultDecodeResponse, defaultMakeRequestWithoutLogs, standardEncode, defaultMakeRequestString)
import Foreign.Generic (decode, encode, class Decode, class Encode)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode)
import Services.Common.API

type DriverProfileScreenCommonState = {
    data :: DriverProfileScreenCommonData,
    props :: DriverProfileScreenCommonProps
}

type DriverProfileScreenCommonProps = {
    driverId :: String,
    rideId :: String
}

type DriverProfileScreenCommonData = {
    certificates :: Array (Array String),
    homeTown :: Maybe String,
    driverName :: String,
    aboutMe :: Maybe String,
    drivingSince :: Maybe Int,
    onboardedAt :: String,
    pledges :: Array String,
    driverStats :: DriverStatSummary,
    languages :: Array String,
    aspirations :: Array String,
    vehicleNum :: Maybe String,
    vechicleVariant :: Variant,
    vehicleTags :: Array String,
    profileImage :: Maybe String,
    images :: Array String,
    displayImages :: Array String,
    imgIdx :: Int,
    topReviews :: Array DriverReview,
    shimmerView :: Boolean
}

initData :: DriverProfileScreenCommonState
initData = {
    data : {
        certificates : [],
        homeTown : Just "",
        driverName : "",
        aboutMe : Just "",
        drivingSince : Just 0,
        onboardedAt : "",
        pledges : [],
        driverStats : driverStatSummary,
        languages : [],
        aspirations : [],
        vehicleNum : Just "",
        vechicleVariant : SEDAN,
        vehicleTags : [],
        profileImage : Just "",
        images : [],
        displayImages : [""],
        imgIdx : 0,
        topReviews : [],
        shimmerView : true
    },
    props : {
        driverId : "",
        rideId : ""
    }
}

driverStatSummary :: DriverStatSummary
driverStatSummary = DriverStatSummary {
    avgRating : Just 0.0,
    numTrips : 0,
    cancellationRate : 0,
    likedByRidersNum : 0
}