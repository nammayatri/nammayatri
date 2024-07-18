{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.FaqScreen.Controller where

import Components.IssueList as IssueListController
import Components.IssueView as IssueViewController
import Data.Array (length) as DA
import Accessor (_driverRatings, _contents, _toLocation, _amount, _driverName, _list, _vehicleNumber, _id, _computedPrice, _shortRideId, _rideRating, _vehicleVariant)
import Components.ErrorModal as ErrorModal
import Components.GenericHeader as GenericHeader
import Components.IndividualRideCard as IndividualRideCard
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.SourceToDestination as SourceToDestination
import Data.Array ((!!), null, filter, reverse, elem)
import Language.Types (STR(..))
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Helpers.Utils (validateEmail,strLenWithSpecificCharacters, isParentView, emitTerminateApp, getTime,toStringJSON, fetchImage, FetchImageFrom(..))
import JBridge (showDialer, hideKeyboardOnNavigation,toast, differenceBetweenTwoUTC)
import Engineering.Helpers.Commons (convertUTCtoISC, getCurrentUTC)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Prelude (class Show, pure, bind, discard, show, unit, map, ($), (<>), (==), void, (&&), (>), (||),(/), not, (>=))
import PrestoDOM (Eval, update, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Resources.Constants (DecodeAddress(..), decodeAddress, getFaresList, getKmMeter, fetchVehicleVariant)
import Screens (ScreenName(..), getScreen)
import Screens.HomeScreen.Transformer (dummyRideAPIEntity)
import Screens.Types ( DeleteStatus(..), IssueInfo, IssueModalType(..))
import Services.API (IssueReportCustomerListItem(..), RideBookingRes(..), FareBreakupAPIEntity(..), RideAPIEntity(..), BookingLocationAPIEntity(..), RideBookingAPIDetails(..), RideBookingListRes(..))
import Services.Config (getSupportNumber)
import Screens.MyRidesScreen.ScreenData (dummyIndividualCard)
import Components.PrimaryEditText as PrimaryEditText
import Components.PrimaryButton as PrimaryButton
import Data.String (length, trim, joinWith, splitAt, toUpper, split, Pattern(..))
import Storage (getValueToLocalStore, KeyStore(..))
import Common.Types.App (LazyCheck(..), CategoryListType)
import Screens.FaqScreen.ScreenData (initData)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.LogEvent (logEvent)
import Foreign.Object (empty)
import ConfigProvider
import Language.Strings( getString)
import Components.IssueList as IssueList
import Data.Function.Uncurried (runFn2)
import Locale.Utils
import Screens.FaqScreen.Transformer
import Screens.FaqScreen.ScreenData (FaqScreenState)
import Components.DropDownCard as DropDownCard
import Common.Types.App (DropDownInfo)


instance showAction :: Show Action where
    show _ = ""

instance loggableAction :: Loggable Action where
    performLog action appId = case action of
      _ -> trackAppActionClick appId (getScreen HELP_AND_SUPPORT_SCREEN) "in_screen" "on_click_done"

data Action = BackPressed
            | GenericHeaderActionController GenericHeader.Action
            | APIFailureActionController ErrorModal.Action
            | AfterRender 
            | DropDownCardActionController DropDownCard.Action
            | PrimaryButtonAC PrimaryButton.Action
            | OpenFavourites PrimaryButton.Action 
            | OpenChangeLanguageScreen PrimaryButton.Action
            | OpenChat String (Maybe String) PrimaryButton.Action
            | OpenSelectRideScreen String (Maybe String) PrimaryButton.Action
            | NoAction
            
data ScreenOutput = GoBack FaqScreenState
                  | GoHome FaqScreenState
                  | GoToFavourites FaqScreenState
                  | ChangeLanguage FaqScreenState
                  | GoToChatScreen String (Maybe String) FaqScreenState
                  | GoToSelectRideScreen String (Maybe String) FaqScreenState

eval :: Action -> FaqScreenState -> Eval Action ScreenOutput FaqScreenState

eval BackPressed state = exit $ GoBack state

eval (GenericHeaderActionController (GenericHeader.PrefixImgOnClick )) state = continueWithCmd state [do pure $ BackPressed]

eval (APIFailureActionController (ErrorModal.PrimaryButtonActionController PrimaryButton.OnClick)) state = exit $ GoBack state

eval (DropDownCardActionController (DropDownCard.OnClick config)) state = do 
  let updatedDropDownList = updateDropDownList state.data.dropDownList config.id 
  continue state {data {dropDownList = updatedDropDownList}}

eval (PrimaryButtonAC (PrimaryButton.OnClick)) state = continue state

eval (OpenFavourites (PrimaryButton.OnClick)) state = exit $ GoToFavourites state

eval (OpenChangeLanguageScreen (PrimaryButton.OnClick)) state = exit $ ChangeLanguage state

eval (OpenChat categoryId optionId (PrimaryButton.OnClick)) state = exit $ GoToChatScreen categoryId optionId state

eval (OpenSelectRideScreen categoryId optionId (PrimaryButton.OnClick)) state = exit $ GoToSelectRideScreen categoryId optionId state

eval _ state = update state

updateDropDownList :: Array DropDownInfo -> String -> Array DropDownInfo
updateDropDownList dropDownList id = 
  let updatedDropDownList = map (\dropDownInfo -> if dropDownInfo.id == id then dropDownInfo {isExpanded = not dropDownInfo.isExpanded} else dropDownInfo{isExpanded = false}) dropDownList
  in updatedDropDownList