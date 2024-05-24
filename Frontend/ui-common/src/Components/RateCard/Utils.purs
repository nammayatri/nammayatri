module Components.RateCard.Utils where

import Prelude
import Common.Types.App (EstimateFares(..), FareList(..), Price(..), BreakupList)
import Data.Maybe (fromMaybe, isJust, Maybe(..), maybe)
import Data.Array as DA
import Mobility.Prelude as MP
import Data.Lens ((^.), view)
import Engineering.Helpers.Accessor as EHA
import JBridge as JB
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Utils as EHU
import Data.Int as DI
import Data.String as DS
import Data.Number as DN
import Language.Strings (getString)
import Language.Types (STR(..))
import Data.Tuple as DT
import Data.Number.Format (fixed, toStringWith)
import Components.ChooseVehicle.Controller as CVC

type StepFare
  = { lLimit :: Number
    , uLimit :: String
    , price :: String
    }

fetchSpecificFare :: Array EstimateFares -> String -> Price
fetchSpecificFare fareBreakup fareType =
  let
    fare = DA.find (\item -> item ^. EHA._title == fareType) fareBreakup
  in
    maybe dummyPriceEntity getPriceEntity fare
  where
    getPriceEntity item = item ^. EHA._priceWithCurrency
    dummyPriceEntity = { amount: 0.0, currency: "INR" }

priceToBeDisplayed :: Price -> String
priceToBeDisplayed price = case price.currency of
  "INR" -> "₹" <> value
  _ -> price.currency <> show value
  where
    value = EHU.getFixedTwoDecimals price.amount

getFareBreakupList :: Array EstimateFares -> Int -> BreakupList
getFareBreakupList fareBreakup maxTip =
  {
    fareList : fareBreakupConstructed,
    fareInfo : fareInfoDescription,
    driverAdditions : map constructDriverAdditions driverAdditionsParsed,
    nightChargeStart : EHC.convertUTCtoISC nightShiftStart "hh a",
    nightChargeEnd : EHC.convertUTCtoISC nightShiftEnd "hh a",
    isNightShift : isNightShift,
    waitingTimeInfo : { 
      freeMinutes: EHU.formatNumber freeWaitingTime.amount Nothing, 
      charge: priceToBeDisplayed waitingCharge <> "/min" 
    }
  }
  where
  fareBreakupConstructed = 
    [ { key: getString $ MIN_FARE_UPTO $ (EHU.formatNumber (baseDistance.amount / 1000.0) Nothing) <> " km", val: baseFare } ]
    <> (map constructExtraFareBreakup extraFareBreakup)
    <> (if congestionCharges.amount > 0.0 then [ { key: getString RUSH_HOUR_CHARGES, val: (EHU.getFixedTwoDecimals congestionCharges.amount) <> "%"}]  else [])
    <> (if tollCharge.amount > 0.0 then [ { key: getString TOLL_CHARGES_ESTIMATED, val: priceToBeDisplayed tollCharge } ] else [])
    <> [ { key: getString PICKUP_CHARGE, val: pickupCharges } ]
    <> [ { key: getString $ WAITING_CHARGE_LIMIT $ EHU.formatNumber freeWaitingTime.amount Nothing, val: priceToBeDisplayed waitingCharge <> "/min" } ]


  fareInfoDescription = 
    [ getString TOTAL_FARE_MAY_CHANGE_DUE_TO_CHANGE_IN_ROUTE
    ]
    <> (if nightShiftRate.amount > 1.0 then [getString $ DAYTIME_CHARGES_APPLIED_AT_NIGHT (toStringWith (fixed 1) (1.0 + nightShiftRate.amount/100.0)) (EHC.convertUTCtoISC nightShiftStart "hh a") (EHC.convertUTCtoISC nightShiftEnd "hh a")] else [])
    <> (if maxTip > 0 then [getString $ TIP_CAN_BE_ADDED $ show maxTip] else [])
    <> (if congestionCharges.amount > 0.0 then [getString $ RUSH_HOURS_DESC $ EHU.getFixedTwoDecimals congestionCharges.amount] else [])

  -- Base fare calculation
  baseDistance = fetchSpecificFare fareBreakup "BASE_DISTANCE"
  baseFare = priceToBeDisplayed $ fetchSpecificFare fareBreakup "BASE_FARE"

  -- Step fare calculation
  extraFareBreakup =
    DA.sortBy compareByLimit
      $ DA.mapMaybe
          ( \item ->
              if MP.startsWith "EXTRA_PER_KM_STEP_FARE_" (item ^. EHA._title) 
                then Just $ parseStepFare item "EXTRA_PER_KM_STEP_FARE_" fareMultiplier
              else Nothing
          )
          fareBreakup

  compareByLimit a b = compare a.lLimit b.lLimit

  fareMultiplier =
    if isNightShift && congestionCharges.amount > 0.0
      then (1.0 + nightShiftRate.amount / 100.0) * (1.0 + congestionCharges.amount / 100.0)
    else if congestionCharges.amount > 0.0 
      then (1.0 + congestionCharges.amount / 100.0)
    else 1.0

  nightShiftStartSeconds = fetchSpecificFare fareBreakup "NIGHT_SHIFT_START_TIME_IN_SECONDS"
  nightShiftStart = EHC.parseSecondsOfDayToUTC $ (DI.round nightShiftStartSeconds.amount)

  nightShiftEndSeconds = fetchSpecificFare fareBreakup "NIGHT_SHIFT_END_TIME_IN_SECONDS"
  nightShiftEnd = EHC.parseSecondsOfDayToUTC $ (DI.round nightShiftEndSeconds.amount + 86400)
  nightShiftEnd_ = EHC.parseSecondsOfDayToUTC $ (DI.round nightShiftEndSeconds.amount) -- For after midnight case

  midNightUtc = EHC.getMidnightUTC unit

  nightShiftRate = fetchSpecificFare fareBreakup "NIGHT_SHIFT_CHARGE_PERCENTAGE"

  isNightShift = (JB.withinTimeRange nightShiftStart nightShiftEnd (EHC.getCurrentUTC ""))
                 || (JB.withinTimeRange midNightUtc nightShiftEnd_ (EHC.getCurrentUTC ""))

  nightCharges =
    if isNightShift then
      JB.withinTimeRange nightShiftStart nightShiftEnd (EHC.convertUTCtoISC (EHC.getCurrentUTC "") "HH:mm:ss")
    else
      false

  -- Pickup charges
  pickupCharges = priceToBeDisplayed $ fetchSpecificFare fareBreakup "DEAD_KILOMETER_FARE"

  --Congestion Charges
  congestionCharges = fetchSpecificFare fareBreakup "CONGESTION_CHARGE_PERCENTAGE"

  -- Toll charges
  tollCharge = fetchSpecificFare fareBreakup "TOLL_CHARGES"

  -- Waiting charges
  freeWaitingTime = fetchSpecificFare fareBreakup "FREE_WAITING_TIME_IN_MINUTES"

  waitingCharge = fetchSpecificFare fareBreakup "WAITING_CHARGE_RATE_PER_MIN"

  parseStepFare :: EstimateFares -> String -> Number -> StepFare
  parseStepFare item prefixString fareMultiplier =
    let
      title = item ^. EHA._title

      price = item ^. EHA._priceWithCurrency

      trimmedTitle = DS.drop (DS.length prefixString) title

      limits = DS.split (DS.Pattern "_") trimmedTitle

      upperlimit = case limits DA.!! 1 of
        Just "Above" -> "+"
        Just limit -> "-" <> EHU.formatNumber ((fromMaybe 0.0 $ DN.fromString limit) / 1000.0) Nothing <> "km"
        Nothing -> ""

      lowerlimit = case (limits DA.!! 0) of
        Just limit -> fromMaybe 0.0 $ DN.fromString limit
        Nothing -> 0.0
    in
      { lLimit: lowerlimit, uLimit: upperlimit, price: priceToBeDisplayed price <> " / km" }

  constructExtraFareBreakup :: StepFare -> FareList
  constructExtraFareBreakup item =
    let
      lowerlimit = EHU.formatNumber ( item.lLimit / 1000.0) Nothing <> "km"
    in
      { key: getString $ FARE_FOR $ lowerlimit <> item.uLimit, val: item.price }

  constructDriverAdditions :: StepFare -> FareList
  constructDriverAdditions item =
    let
      lowerlimit = EHU.formatNumber (item.lLimit / 1000.0) Nothing <> "km"
    in
      { key: lowerlimit <> item.uLimit, val: item.price }

  driverAdditionsParsed =
    DA.sortBy compareByLimit
      $ DA.mapMaybe
          ( \item ->
              if MP.startsWith "DRIVER_EXTRA_FEE_BOUNDS_STEP_MAX_FEE_" (item ^. EHA._title) && (item ^. EHA._title) /= "DRIVER_EXTRA_FEE_BOUNDS_STEP_MAX_FEE_0_Above"
                then Just $ parseStepFare item "DRIVER_EXTRA_FEE_BOUNDS_STEP_MAX_FEE_" 1.0
              else Nothing
          )
          fareBreakup
