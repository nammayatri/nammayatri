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
import Helpers.Utils (toStringJSON, fetchImage, FetchImageFrom(..))
import JBridge (showDialer, hideKeyboardOnNavigation,toast, differenceBetweenTwoUTC)
import Engineering.Helpers.Commons (convertUTCtoISC, getCurrentUTC)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Resources.Constants (DecodeAddress(..), decodeAddress, getFaresList, getKmMeter, fetchVehicleVariant)
import Screens (ScreenName(..), getScreen)
import Screens.HomeScreen.Transformer (dummyRideAPIEntity)
import Screens.Types (HelpAndSupportScreenState, DeleteStatus(..), IssueInfo, IssueModalType(..))
import Services.API (IssueReportCustomerListItem(..), RideBookingRes(..), FareBreakupAPIEntity(..), RideAPIEntity(..), BookingLocationAPIEntity(..), RideBookingAPIDetails(..), RideBookingListRes(..))
import Screens.MyRidesScreen.ScreenData (dummyIndividualCard)
import Data.String 
import Storage (getValueToLocalStore, KeyStore(..))
import Common.Types.App (LazyCheck(..), CategoryListType)
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (empty)
import Language.Strings( getString)
import Components.IssueList as IssueList
import Data.Function.Uncurried (runFn2)
import Locale.Utils

reportIssueMessageTransformer :: String -> String 
reportIssueMessageTransformer message = 
  let config = getAppConfig appConfig
      keyValuePairs = [ { key: Pattern "{#SUPPORT_EMAIL#}", value: Replacement config.appData.supportMail }
                      , { key: Pattern "{#MERCHANT#}", value: Replacement config.appData.name }
                      , { key: Pattern "\\n", value : Replacement "<br>"} ]
  in foldl (\acc { key, value } -> replaceAll key value acc) message keyValuePairs



myRideListTransform :: HelpAndSupportScreenState -> Array RideBookingRes -> Array HelpAndSupportScreenState
myRideListTransform state listRes = filter (\item -> (item.data.status == "COMPLETED")) (map(\(RideBookingRes ride) ->
    let
    (RideAPIEntity rideDetails) = (fromMaybe dummyRideAPIEntity (ride.rideList !!0))
    baseDistanceVal = (getKmMeter (fromMaybe 0 (rideDetails.chargeableRideDistance)))
    updatedFareList = getFaresList ride.fareBreakup baseDistanceVal
    config = getAppConfig appConfig
      in  {
        data:{
          date : (convertUTCtoISC (ride.createdAt) "ddd, Do MMM"),
          time : (convertUTCtoISC (fromMaybe (ride.createdAt) ride.rideStartTime ) "h:mm A"),
          source: decodeAddress (Booking ride.fromLocation),
          destination: (decodeAddress (Booking (ride.bookingDetails ^._contents^._toLocation))),
          rating: (fromMaybe 0 ((fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^. _rideRating)),
          driverName :((fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^. _driverName) ,
          totalAmount : (config.currency <> " " <> show (fromMaybe (0) ((fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^. _computedPrice))),
          status : (ride.status),
          isNull : false,
          rideStartTime : (convertUTCtoISC (fromMaybe "" ride.rideStartTime )"h:mm A"),
          rideEndTime : (convertUTCtoISC (fromMaybe "" ride.rideEndTime )"h:mm A"),
          vehicleNumber : ((fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^._vehicleNumber),
          rideId : ((fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^._id),
          tripId : ((fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^._shortRideId),
          bookingId : ride.id,
          faresList : updatedFareList,
          config : config,
          email : "",
          description : "",
          accountStatus : ACTIVE,
          vehicleVariant : fetchVehicleVariant ((fromMaybe dummyRideAPIEntity (ride.rideList !!0) )^._vehicleVariant),
          logField : empty,
          issueList : state.data.issueList,
          resolvedIssueList : state.data.resolvedIssueList,
          ongoingIssueList : state.data.ongoingIssueList,
          issueListType : HELP_AND_SUPPORT_SCREEN_MODAL,
          categories : state.data.categories,
          merchantExoPhone : ride.merchantExoPhone
          },
      props : {
        apiFailure : false
      , isCallConfirmation : false
      , showDeleteAccountView : false
      , btnActive : false
      , needIssueListApiCall : false
      }
      }) listRes)


dummyFareBreakUp :: FareBreakupAPIEntity
dummyFareBreakUp = FareBreakupAPIEntity{amount: 0,description: ""}

isEmailPresent :: LazyCheck -> Boolean
isEmailPresent _ = not ( getValueToLocalStore USER_EMAIL == "__failed" || getValueToLocalStore USER_EMAIL == "(null)" )

getApiIssueList :: Array IssueReportCustomerListItem -> Array IssueInfo
getApiIssueList issueList = (map (\(IssueReportCustomerListItem issue) -> {
   issueReportId : issue.issueReportId,
   status : issue.status,
   category : if (getLanguageLocale languageKey == "EN_US")
                then
                  joinWith " " (map (\catName ->
                    let { before, after } = splitAt 1 catName
                    in (toUpper before <> after)
                  ) (split (Pattern " ") issue.category))
                else issue.category,
   createdAt : getExactTime (runFn2 differenceBetweenTwoUTC (getCurrentUTC "") (issue.createdAt)) issue.createdAt
}) issueList)

getExactTime :: Int -> String -> String 
getExactTime sec createdAt = 
   if sec >= 86400 then convertUTCtoISC createdAt "DD/MM/YYYY"
   else 
    let {base , suffix}=    if sec >= 3600 then {base : sec / 3600, suffix : getString HOURS_AGO}
                            else if sec >= 60 then {base : sec / 60,  suffix : getString MIN_AGO}
                            else {base: sec , suffix: getString SEC_AGO}
    in toStringJSON (base) <> " " <> suffix

getUpdatedIssueList :: Array String -> Array IssueInfo -> Array IssueInfo
getUpdatedIssueList statusList list = filter (\(issue) -> elem issue.status statusList ) list 

topicsList :: HelpAndSupportScreenState -> Array CategoryListType
topicsList state =  (if state.data.config.feature.enableSelfServe 
  then 
    state.data.categories 
  else 
    [{ categoryAction : "CONTACT_US"
    , categoryName : getString FOR_OTHER_ISSUES_WRITE_TO_US
    , categoryImageUrl : fetchImage FF_COMMON_ASSET "ny_ic_clip_board"
    , categoryId : "5"
    },
    { categoryAction : "CALL_SUPPORT"
    , categoryName : getString CONTACT_SUPPORT
    , categoryImageUrl : fetchImage FF_COMMON_ASSET "ny_ic_help"
    , categoryId : "6"
    }]
  ) <> if state.data.config.showDeleteAccount 
          then 
            [{ categoryAction : "DELETE_ACCOUNT"
            , categoryName : getString REQUEST_TO_DELETE_ACCOUNT
            , categoryImageUrl : fetchImage FF_COMMON_ASSET "ny_ic_delete_account"
            , categoryId : "7"
            }] 
          else 
            []

reportsList :: HelpAndSupportScreenState -> Array CategoryListType
reportsList state = []
  <> if null state.data.ongoingIssueList then [] else [
        { categoryAction : "REPORTED"
        , categoryName : getString REPORTED <> " : " <> (toStringJSON (DA.length (state.data.ongoingIssueList)))
        , categoryImageUrl : fetchImage FF_COMMON_ASSET "ny_ic_reported"
        , categoryId : "1"
        }]
  <> if null state.data.resolvedIssueList then [] else [
        { categoryAction : "CLOSED"
        , categoryName : getString RESOLVED <> " : " <> (toStringJSON (DA.length (state.data.resolvedIssueList)))
        , categoryImageUrl : fetchImage FF_COMMON_ASSET "ny_ic_resolved"
        , categoryId : "2"
        }]