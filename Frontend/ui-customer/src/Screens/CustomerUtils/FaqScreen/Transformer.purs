{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.FaqScreen.Transformer where

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
import Helpers.Utils (toStringJSON, fetchImage, FetchImageFrom(..), withinTimeRange)
import JBridge (showDialer, hideKeyboardOnNavigation, toast, differenceBetweenTwoUTC)
import Engineering.Helpers.Commons (convertUTCtoISC, getCurrentUTC)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import PrestoDOM (Eval, update, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Resources.Constants (DecodeAddress(..), decodeAddress, getFaresList, getKmMeter, fetchVehicleVariant, getFareFromArray)
import Screens (ScreenName(..), getScreen)
import Screens.HomeScreen.Transformer (dummyRideAPIEntity)
import Screens.Types (DeleteStatus(..), IssueInfo, IssueModalType(..))
import Services.API (IssueReportCustomerListItem(..), RideBookingRes(..), FareBreakupAPIEntity(..), RideAPIEntity(..), BookingLocationAPIEntity(..), RideBookingAPIDetails(..), RideBookingListRes(..), RideBookingDetails(..))
import Screens.MyRidesScreen.ScreenData (dummyIndividualCard, dummyBookingDetails)
import Data.String
import Storage (getValueToLocalStore, KeyStore(..))
import Common.Types.App (LazyCheck(..), CategoryListType, DropDownInfo, StringType)
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (empty)
import Language.Strings (getString)
import Components.IssueList as IssueList
import Data.Function.Uncurried (runFn2)
import Locale.Utils
import Screens.FaqScreen.ScreenData (FaqScreenState)
import Engineering.Helpers.BackTrack (getState)
import Types.App (GlobalState(..))
import Services.Backend as Remote
import Data.Int as INT
import Data.Number (fromString)
import Foreign (unsafeToForeign)
import Data.Array as DA
import Data.String as DS

topicsList :: FaqScreenState -> Array CategoryListType
topicsList state =
  state.data.categories
  <> [ { categoryAction: Just "DELETE_ACCOUNT"
          , categoryName: getString REQUEST_TO_DELETE_ACCOUNT
          , categoryImageUrl: Just $ fetchImage FF_COMMON_ASSET "ny_ic_delete_account"
          , categoryId: "7"
          , isRideRequired: false
          , maxAllowedRideAge: Nothing
          , categoryType: "Category"
          }
        ]

-- dropDownCardInfoList :: Array DropDownInfo
-- dropDownCardInfoList =
--   [ { title: "How do I create a Namma Yatri account?"
--     , description: "{SUBPART}{HEADING}{!!!}heading 1{SUBPART}{BODY}{!!!}body1"
--     , id: "1"
--     , isExpanded: false
--     }
--   , { title: "Can I change my registered mobile number?"
--     -- , description: "<b> Section 1 Header </b> <br> <span style='color:#6D7280'> Namma Yatri does not support editing the registered mobile number currently. If you wish to use another number, please logout from the current account and register with a new number </span> <br> <br> <b> Section 2 Header </b> <br> <span style='color:#6D7280'> Namma Yatri does not support editing the registered mobile number currently. If you wish to use another number, please logout from the current account and register with a new number </span> "
--     , description: "{SUBPART}{HEADING}{!!!}Section 1 Header {SUBPART}{BODY}{!!!} Namma Yatri does not support editing the registered mobile number currently. If you wish to use another number, please logout from the current account and register with a new number. {SUBPART}{HEADING}{!!!}Section 2 Header {SUBPART}{BODY}{!!!} Namma Yatri does not support editing the registered mobile number currently. If you wish to use another number, please logout from the current account and register with a new number."
--     , id: "2"
--     , isExpanded: false
--     }
--   , { title: "Does Namma Yatri have a referral program?"
--     , description: "{SUBPART}{HEADING}{!!!}heading 3{SUBPART}{BODY}{!!!}body3"
--     , id: "3"
--     , isExpanded: false
--     }
--   , { title: "I am unable to log in to my account"
--     , description: "{SUBPART}{HEADING}{!!!}heading 4{SUBPART}{BODY}{!!!}body4"
--     , id: "4"
--     , isExpanded: false
--     }
--   ]

dropDownStringTransform :: String -> StringType
dropDownStringTransform str =
  let 
    newStrings = DS.split(DS.Pattern ("{!!!}")) str
    value' = fromMaybe "" (newStrings DA.!! 1)
    type' = fromMaybe "" (newStrings DA.!! 0)
  in 
    {value: value', type: type' }

dropDownStringTypes :: String -> Array StringType
dropDownStringTypes str = 
  let
    newStrings = DS.split(DS.Pattern ("{SUBPART}")) str
  in 
    map dropDownStringTransform newStrings