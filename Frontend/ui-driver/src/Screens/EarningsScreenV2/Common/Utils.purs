module Screens.EarningsScreen.Common.Utils where

import Prelude
import JBridge (getCurrentDate)
import Services.API
import Effect
import LocalStorage.Cache (getValueFromCache, setValueToCache)
import DecodeUtil
import Data.Array (filter, head, elem, foldr)
import Screens.EarningsScreen.Common.Types
import Foreign.Object
import Data.Maybe
import Control.Monad.Except (runExcept)
import Foreign.Generic (class Decode, decode, encode)
import JBridge (getKeyInSharedPrefKeys)
import Data.Either
import Engineering.Helpers.Commons as EHC
import Effect.Ref (read, new, write)
import Constants.Configs
import Debug


formateDate :: String -> String
formateDate rideDate = if rideDate == getCurrentDate then "Today" else rideDate

getDateFromCurrentDate :: String -> String
getDateFromCurrentDate rideDate = if rideDate == "Today" then getCurrentDate else rideDate

getRideData :: String -> Maybe RideData
getRideData date = lookup date $ getValueFromCache "RIDE_DATA" getSummary

getDatesListV2 :: String -> String -> String -> Boolean -> Array String
getDatesListV2 todaysDate fromDate toDate isCurrentWeek = do
  if fromDate /= "" && toDate /= "" && not isCurrentWeek
  then map (\obj -> EHC.convertUTCtoISC obj.utcDate "YYYY-MM-DD") $ EHC.getPastDays toDate 7
  else do
    let dayOfWeek = EHC.getDayOfWeek (EHC.getDayName todaysDate) + 1
    map (\obj -> EHC.convertUTCtoISC obj.utcDate "YYYY-MM-DD") $ EHC.getPastDays todaysDate dayOfWeek

updateRideDatas :: Array RidesSummary -> Array RidesInfo -> String -> Boolean -> Effect Unit
updateRideDatas resp list givenDate noListFetched = do 
  let (existing :: Object RideData) = getValueFromCache "RIDE_DATA" getSummary
      updatedData = foldr (\(RidesSummary rideSummary) acc -> do
                let summary = case lookup rideSummary.rideDate acc of
                                Nothing -> fromRidesSummaryToRideData (RidesSummary rideSummary)
                                Just val -> updateRideData (RidesSummary rideSummary) val
                insert rideSummary.rideDate summary{noListFetched = noListFetched} acc
                ) existing resp
      finalData = foldr (\(RidesInfo rides) acc -> do
                let date = EHC.convertUTCtoISC rides.createdAt "YYYY-MM-DD"
                    rideData = case lookup date acc of
                                Nothing -> fromRidesInfoToRideData (RidesInfo rides) date
                                Just val -> if val.noSummaryFound then updateRideDataFromRideInfo (RidesInfo rides) val else updateRideInfo (RidesInfo rides) val
                insert date rideData{noListFetched = noListFetched} acc
                ) updatedData list
      _ = setValueToCache "RIDE_DATA" (if isNothing $ lookup givenDate finalData then insert givenDate (dummyRideData givenDate) finalData else finalData) (\val -> stringifyJSON $ encode val )
  pure unit

getSummary :: (String -> Object RideData)
getSummary =  
  (\key -> 
      let stringified  = getKeyInSharedPrefKeys key 
      in case hush $ runExcept $ decode $ parseJSON $ if elem stringified ["__failed", "(null)", ""] then "{}" else stringified of
          Just items -> items
          Nothing -> setValueToCache "RIDE_DATA" empty (\val -> stringifyJSON $ encode val ))

updateRideData :: RidesSummary -> RideData -> RideData
updateRideData (RidesSummary rideSummary) rideObject = rideObject{
  earnings = rideSummary.earnings,
  earningsWithCurrency = rideSummary.earningsWithCurrency,
  rideDistance = rideSummary.rideDistance,
  rideDate = rideSummary.rideDate,
  noOfRides = rideSummary.noOfRides,
  noSummaryFound = false,
  rideDistanceWithUnit = rideSummary.rideDistanceWithUnit
}

updateRideInfo :: RidesInfo -> RideData -> RideData
updateRideInfo (RidesInfo rideInfo) rideObject = 
  let filtered = (filter (\(RidesInfo existingRide) -> existingRide.id == rideInfo.id) rideObject.list) 
  in case head filtered of
        Nothing ->  rideObject{ list = rideObject.list <> [(RidesInfo rideInfo)]}
        Just _ -> rideObject

updateRideDataFromRideInfo :: RidesInfo -> RideData -> RideData
updateRideDataFromRideInfo (RidesInfo rideInfo) rideObject = rideObject {
  earnings = rideObject.earnings + rideInfo.estimatedBaseFare,
  earningsWithCurrency = rideObject.earningsWithCurrency{amount = rideObject.earningsWithCurrency.amount + rideInfo.estimatedBaseFareWithCurrency.amount},
  rideDistance = rideObject.rideDistance + rideInfo.estimatedDistance,
  noOfRides = rideObject.noOfRides + 1,
  rideDistanceWithUnit = rideInfo.estimatedDistanceWithUnit,
  noListFetched = false,
  noSummaryFound = true,
  list= [(RidesInfo rideInfo)]
}

dummyRideData :: String -> RideData
dummyRideData date =  {
  earnings : 0,
  earningsWithCurrency : dummyPrice,
  rideDistance : 0,
  rideDate : date,
  noOfRides : 1,
  rideDistanceWithUnit : dummyDistance,
  noSummaryFound : false,
  noListFetched : false,
  noRidesTaken : true,
  list: []
}


fromRidesInfoToRideData :: RidesInfo -> String -> RideData
fromRidesInfoToRideData (RidesInfo rideInfo) date = {
  earnings : if rideInfo.status == "CANCELLED" then 0 else rideInfo.estimatedBaseFare,
  earningsWithCurrency : if rideInfo.status == "CANCELLED" then dummyPrice else rideInfo.estimatedBaseFareWithCurrency,
  rideDistance : if rideInfo.status == "CANCELLED" then 0 else rideInfo.estimatedDistance,
  rideDate : date,
  noOfRides : 1,
  rideDistanceWithUnit : if rideInfo.status == "CANCELLED" then dummyDistance else rideInfo.estimatedDistanceWithUnit,
  noSummaryFound : true,
  noRidesTaken : false,
  noListFetched : false,
  list: [(RidesInfo rideInfo)]
}

fromRidesSummaryToRideData :: RidesSummary -> RideData
fromRidesSummaryToRideData (RidesSummary rideSummary) = {
  earnings : rideSummary.earnings,
  earningsWithCurrency : rideSummary.earningsWithCurrency,
  rideDistance : rideSummary.rideDistance,
  rideDate : rideSummary.rideDate,
  noOfRides : rideSummary.noOfRides,
  rideDistanceWithUnit : rideSummary.rideDistanceWithUnit,
  noSummaryFound : false,
  noListFetched : false,
  noRidesTaken : false,
  list: []
}

fromRideDataToRidesSummary :: RideData -> RidesSummary
fromRideDataToRidesSummary rideData = RidesSummary {
  earnings : rideData.earnings,
  earningsWithCurrency : rideData.earningsWithCurrency,
  rideDistance : rideData.rideDistance,
  rideDate : rideData.rideDate,
  noOfRides : rideData.noOfRides,
  rideDistanceWithUnit : rideData.rideDistanceWithUnit
}