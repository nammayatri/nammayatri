{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.SelectFaqScreen.Transformer where

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
import Common.Types.App (LazyCheck(..), CategoryListType)
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (empty)
import Language.Strings (getString)
import Components.IssueList as IssueList
import Data.Function.Uncurried (runFn2)
import Locale.Utils
import Screens.SelectFaqScreen.ScreenData (SelectFaqScreenState)
import Engineering.Helpers.BackTrack (getState)
import Types.App (GlobalState(..))
import Services.Backend as Remote
import Data.Int as INT
import Data.Number (fromString)
import Foreign (unsafeToForeign)

topicsList :: SelectFaqScreenState -> Array CategoryListType
topicsList state =
  ( if state.data.config.feature.enableSelfServe then
      state.data.categories
    else
      [ { categoryAction: Just "CONTACT_US"
        , categoryName: getString FOR_OTHER_ISSUES_WRITE_TO_US
        , categoryImageUrl: Just $ fetchImage FF_COMMON_ASSET "ny_ic_clip_board"
        , categoryId: "5"
        , isRideRequired: false
        , maxAllowedRideAge: Nothing
        , categoryType: "Category"
        }
      , { categoryAction: Just "CALL_SUPPORT"
        , categoryName: getString CONTACT_SUPPORT
        , categoryImageUrl: Just $ fetchImage FF_COMMON_ASSET "ny_ic_help"
        , categoryId: "6"
        , isRideRequired: false
        , maxAllowedRideAge: Nothing
        , categoryType: "Category"
        }
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
          }
        ]
      else
        []

