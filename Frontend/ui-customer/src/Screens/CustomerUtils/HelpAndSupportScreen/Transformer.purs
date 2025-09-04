{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.HelpAndSupportScreen.Transformer where

import Prelude
import Data.String (Pattern(..), Replacement(..), replaceAll)
import ConfigProvider
import Data.Foldable (foldl)
import Data.Array (length) as DA
import Accessor (_driverRatings, _contents, _toLocation, _amount, _driverName, _list, _vehicleNumber, _id, _computedPrice, _shortRideId, _rideRating, _vehicleVariant)
import Data.Array ((!!), null, filter, elem)
import Language.Types (STR(..))
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Helpers.Utils (toStringJSON, fetchImage, FetchImageFrom(..), withinTimeRange,fetchVehicleVariant)
import JBridge (showDialer, hideKeyboardOnNavigation, differenceBetweenTwoUTC)
import Engineering.Helpers.Commons (convertUTCtoISC, getCurrentUTC)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import PrestoDOM (Eval, update, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Resources.Constants (DecodeAddress(..), decodeAddress, getFaresList, getKmMeter, getFareFromArray)
import Screens (ScreenName(..), getScreen)
import Screens.HomeScreen.Transformer (dummyRideAPIEntity)
import Screens.Types (DeleteStatus(..), IssueInfo, IssueModalType(..))
import Services.API (IssueReportCustomerListItem(..), RideBookingRes(..), FareBreakupAPIEntity(..), RideAPIEntity(..), BookingLocationAPIEntity(..), RideBookingAPIDetails(..), RideBookingListRes(..), RideBookingDetails(..))
import Screens.MyRidesScreen.ScreenData (dummyIndividualCard, dummyBookingDetails)
import Data.String
import Storage (getValueToLocalStore, KeyStore(..))
import Common.Types.App (LazyCheck(..), CategoryListType)
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (empty)
import Language.Strings (getString)
import Components.IssueList as IssueList
import Data.Function.Uncurried (runFn2)
import Locale.Utils
import Screens.HelpAndSupportScreen.ScreenData (HelpAndSupportScreenState)
import Engineering.Helpers.BackTrack (getState)
import RemoteConfig as RC
import Types.App (GlobalState(..))
import Services.Backend as Remote
import Data.Int as INT
import Data.Number (fromString)
import Foreign (unsafeToForeign)

reportIssueMessageTransformer :: String -> String
reportIssueMessageTransformer message =
  let
    config = getAppConfig appConfig

    keyValuePairs =
      [ { key: Pattern "{#SUPPORT_EMAIL#}", value: Replacement config.appData.supportMail }
      , { key: Pattern "{#MERCHANT#}", value: Replacement config.appData.name }
      , { key: Pattern "\\n", value: Replacement "<br>" }
      ]
  in
    foldl (\acc { key, value } -> replaceAll key value acc) message keyValuePairs

rideInfoTransformer :: RideBookingRes -> String -> String
rideInfoTransformer (RideBookingRes resp) message =
  let
    config = getAppConfig appConfig

    (RideBookingAPIDetails bookingDetails) = resp.bookingDetails

    (RideBookingDetails contents) = bookingDetails.contents

    (RideAPIEntity ride) = fromMaybe dummyRideAPIEntity (resp.rideList !! 0)

    differenceOfDistance = fromMaybe 0 contents.estimatedDistance - (fromMaybe 0 ride.chargeableRideDistance)

    timeVal = (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "HH:mm:ss")

    nightChargesVal = (withinTimeRange "22:00:00" "5:00:00" timeVal)

    estimatedDistance = fromMaybe 0 contents.estimatedDistance / 1000

    finalDistance = fromMaybe 0 ride.chargeableRideDistance / 1000

    distanceDifference = differenceOfDistance / 1000

    estimatedFare = resp.estimatedFare

    finalFare = INT.round $ fromMaybe 0.0 $ fromString (show (fromMaybe 0 ride.computedPrice))

    fareDifference = resp.estimatedFare - finalFare

    fareBreakup = resp.fareBreakup

    tollCharges = getFareFromArray fareBreakup "TOLL_CHARGES"

    driverPickupCharges = getFareFromArray fareBreakup "DEAD_KILOMETER_FARE"

    tipAdded = getFareFromArray fareBreakup "CUSTOMER_SELECTED_FARE"

    driverAdditions = getFareFromArray fareBreakup "DRIVER_SELECTED_FARE"

    currency = getCurrency appConfig

    keyValuePairs =
      [ { key: Pattern "{#ESTIMATED_DISTANCE#}", value: Replacement (show estimatedDistance) }
      , { key: Pattern "{#FINAL_DISTANCE#}", value: Replacement (show finalDistance) }
      , { key: Pattern "{#DISTANCE_DIFFERENCE#}", value: Replacement (show distanceDifference) }
      , { key: Pattern "{#ESTIMATED_FARE#}", value: Replacement (currency <> (show estimatedFare)) }
      , { key: Pattern "{#FINAL_FARE#}", value: Replacement (currency <> (show finalFare)) }
      , { key: Pattern "{#FARE_DIFFERENCE#}", value: Replacement (currency <> (show fareDifference)) }
      , { key: Pattern "{#FARE_CORRELATION#}", value: Replacement (if finalFare == estimatedFare then "remained same" else if finalFare > estimatedFare then ("was increased by ₹" <> show fareDifference) else ("was decreased by ₹" <> show fareDifference)) }
      , { key: Pattern "{#DISTANCE_CORRELATION#}", value: Replacement (if finalDistance == estimatedDistance then "no change" else if finalDistance > estimatedDistance then ("a " <> show distanceDifference <> " km increase") else ("a " <> show distanceDifference <> " km decrease")) }
      , { key: Pattern "{#FARE_ARROW#}", value: Replacement (if finalFare == estimatedFare then "" else if finalFare > estimatedFare then " ↑" else " ↓") }
      , { key: Pattern "{#DISTANCE_ARROW#}", value: Replacement (if finalDistance == estimatedDistance then "" else if finalDistance > estimatedDistance then " ↑" else " ↓") }
      , { key: Pattern "{#DRIVER_PICKUP_CHARGES#}", value: Replacement (currency <> (show driverPickupCharges)) }
      , { key: Pattern "{#TOLL_CHARGES#}", value: Replacement (currency <> (show tollCharges)) }
      , { key: Pattern "{#TIP_ADDED#}", value: Replacement (currency <> (show tipAdded)) }
      , { key: Pattern "{#DRIVER_ADDITIONS#}", value: Replacement (currency <> (show driverAdditions)) }
      ]
  in
    foldl (\acc { key, value } -> replaceAll key value acc) message keyValuePairs

myRideListTransform :: HelpAndSupportScreenState -> Array RideBookingRes -> Array HelpAndSupportScreenState
myRideListTransform state listRes =
  filter (\item -> (item.data.status == "COMPLETED"))
    ( map
        ( \(RideBookingRes ride) ->
            let
              (RideAPIEntity rideDetails) = (fromMaybe dummyRideAPIEntity (ride.rideList !! 0))
              (RideBookingAPIDetails bookingDetails) = ride.bookingDetails
              baseDistanceVal = (getKmMeter (fromMaybe 0 (rideDetails.chargeableRideDistance)))
              rideStatus = fromMaybe "" (ride.rideList !! 0 <#> \(RideAPIEntity ride) -> ride.status)
              updatedFareList = getFaresList ride.fareBreakup baseDistanceVal (bookingDetails.fareProductType == "OneWaySpecialZoneAPIDetails")

              config = getAppConfig appConfig
            in
              { data:
                  { date: (convertUTCtoISC (ride.createdAt) "ddd, Do MMM")
                  , time: (convertUTCtoISC (fromMaybe (ride.createdAt) ride.rideStartTime) "h:mm A")
                  , source: decodeAddress (Booking ride.fromLocation)
                  , destination: (decodeAddress (Booking (fromMaybe dummyBookingDetails (ride.bookingDetails ^. _contents ^. _toLocation))))
                  , rating: (fromMaybe 0 ((fromMaybe dummyRideAPIEntity (ride.rideList !! 0)) ^. _rideRating))
                  , driverName: ((fromMaybe dummyRideAPIEntity (ride.rideList !! 0)) ^. _driverName)
                  , totalAmount: (config.currency <> " " <> show (fromMaybe (0) ((fromMaybe dummyRideAPIEntity (ride.rideList !! 0)) ^. _computedPrice)))
                  , status: (ride.status)
                  , isNull: false
                  , rideStartTime: (convertUTCtoISC (fromMaybe "" ride.rideStartTime) "h:mm A")
                  , rideStartTimeUTC: fromMaybe "" ride.rideStartTime
                  , rideEndTimeUTC: fromMaybe "" ride.rideEndTime
                  , rideEndTime: (convertUTCtoISC (fromMaybe "" ride.rideEndTime) "h:mm A")
                  , vehicleNumber: ((fromMaybe dummyRideAPIEntity (ride.rideList !! 0)) ^. _vehicleNumber)
                  , rideId: ((fromMaybe dummyRideAPIEntity (ride.rideList !! 0)) ^. _id)
                  , tripId: ((fromMaybe dummyRideAPIEntity (ride.rideList !! 0)) ^. _shortRideId)
                  , bookingId: ride.id
                  , faresList: updatedFareList
                  , config: config
                  , email: ""
                  , isLoading: false
                  , fromScreen: state.data.fromScreen
                  , description: ""
                  , accountStatus: ACTIVE
                  , vehicleVariant: fetchVehicleVariant ((fromMaybe dummyRideAPIEntity (ride.rideList !! 0)) ^. _vehicleVariant)
                  , logField: empty
                  , issueList: state.data.issueList
                  , resolvedIssueList: state.data.resolvedIssueList
                  , ongoingIssueList: state.data.ongoingIssueList
                  , issueListType: HELP_AND_SUPPORT_SCREEN_MODAL
                  , categories: state.data.categories
                  , merchantExoPhone: ride.merchantExoPhone
                  , isFaqListEmpty: state.data.isFaqListEmpty
                  , rideCreatedAt: ride.createdAt
                  , rideStatus: rideStatus
                  }
              , props:
                  { apiFailure: false
                  , isCallConfirmation: false
                  , showDeleteAccountView: false
                  , btnActive: false
                  , needIssueListApiCall: false
                  }
              }
        )
        listRes
    )

isEmailPresent :: LazyCheck -> Boolean
isEmailPresent _ = not (getValueToLocalStore USER_EMAIL == "__failed" || getValueToLocalStore USER_EMAIL == "(null)")

getApiIssueList :: Array IssueReportCustomerListItem -> Array IssueInfo
getApiIssueList issueList = map (\(IssueReportCustomerListItem issue) -> issueInfoT (IssueReportCustomerListItem issue)) issueList
  where
  issueInfoT :: IssueReportCustomerListItem -> IssueInfo
  issueInfoT (IssueReportCustomerListItem issue) =
    let
      issueReportId' = issue.issueReportId

      status' = issue.status

      category' =
        if getLanguageLocale languageKey == "EN_US" then
          joinWith " " $ map (\catName -> categoriesT catName) (split (Pattern " ") issue.category)
        else
          issue.category

      createdAt' = getExactTime (runFn2 differenceBetweenTwoUTC (getCurrentUTC "") (issue.createdAt)) issue.createdAt

      issueReportShortId' = issue.issueReportShortId
    in
      { issueReportId: issueReportId'
      , status: status'
      , category: category'
      , createdAt: createdAt'
      , issueReportShortId: issueReportShortId'
      , optionLabel: issue.optionLabel
      , rideId: issue.rideId
      }

  categoriesT :: String -> String
  categoriesT catName =
    let
      { before, after } = splitAt 1 catName
    in
      (toUpper before <> after)

getExactTime :: Int -> String -> String
getExactTime sec createdAt =
  if sec >= 86400 then
    convertUTCtoISC createdAt "DD/MM/YYYY"
  else
    let
      { base, suffix } =
        if sec >= 3600 then
          { base: sec / 3600, suffix: getString HOURS_AGO }
        else if sec >= 60 then
          { base: sec / 60, suffix: getString MIN_AGO }
        else
          { base: sec, suffix: getString SEC_AGO }
    in
      toStringJSON (base) <> " " <> suffix

getUpdatedIssueList :: Array String -> Array IssueInfo -> Array IssueInfo
getUpdatedIssueList statusList list = filter (\(issue) -> elem issue.status statusList) list

topicsList :: HelpAndSupportScreenState -> Array CategoryListType
topicsList state =
  let 
    customerLocation = getValueToLocalStore CUSTOMER_LOCATION
    enableContactSupport = RC.getEnableContactSupport customerLocation
    callSupportCategory = { categoryAction: Just "CALL_SUPPORT"
                          , categoryName: getString CONTACT_SUPPORT
                          , categoryImageUrl: Just $ fetchImage FF_COMMON_ASSET "ny_ic_help"
                          , categoryId: "6"
                          , isRideRequired: false
                          , maxAllowedRideAge: Nothing
                          , allowedRideStatuses : Nothing
                          , categoryType: "Category"
                          }
  in
    ( if state.data.config.feature.enableSelfServe then
        state.data.categories <> (if enableContactSupport then [callSupportCategory] else [])
      else
        [ { categoryAction: Just "CONTACT_US"
          , categoryName: getString FOR_OTHER_ISSUES_WRITE_TO_US
          , categoryImageUrl: Just $ fetchImage FF_COMMON_ASSET "ny_ic_clip_board"
          , categoryId: "5"
          , isRideRequired: false
          , maxAllowedRideAge: Nothing
          , allowedRideStatuses : Nothing
          , categoryType: "Category"
          }
        , callSupportCategory
        ]
    )
    <> if state.data.config.showDeleteAccount then
        [ { categoryAction: Just "DELETE_ACCOUNT"
          , categoryName: getString REQUEST_TO_DELETE_ACCOUNT
          , categoryImageUrl: Just $ fetchImage FF_COMMON_ASSET "ny_ic_delete_account"
          , categoryId: "7"
          , isRideRequired: false
          , maxAllowedRideAge: Nothing
          , categoryType: "Category"
          , allowedRideStatuses: Nothing
          }
        ]
      else
        []

reportsList :: HelpAndSupportScreenState -> Array CategoryListType
reportsList state =
  []
    <> ( if null state.data.ongoingIssueList then
          []
        else
          [ { categoryAction: Just "REPORTED"
            , categoryName: getString ACTIVE_STR <> " : " <> (toStringJSON (DA.length (state.data.ongoingIssueList)))
            , categoryImageUrl: Just $ fetchImage FF_COMMON_ASSET "ny_ic_reported"
            , categoryId: "1"
            , isRideRequired: false
            , maxAllowedRideAge: Nothing
            , allowedRideStatuses: Nothing
            , categoryType: "Category"
            }
          ]
      )
    <> ( if null state.data.resolvedIssueList then
          []
        else
          [ { categoryAction: Just "CLOSED"
            , categoryName: getString RESOLVED <> " : " <> (toStringJSON (DA.length (state.data.resolvedIssueList)))
            , categoryImageUrl: Just $ fetchImage FF_COMMON_ASSET "ny_ic_resolved"
            , categoryId: "2"
            , isRideRequired: false
            , maxAllowedRideAge: Nothing
            , allowedRideStatuses: Nothing
            , categoryType: "Category"
            }
          ]
      )
