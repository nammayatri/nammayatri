{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HelpAndSupportScreen.ScreenData where

import Screens.Types
import Common.Types.App
import MerchantConfig.DefaultConfig as DC
import ConfigProvider
import Data.Maybe(Maybe(..))
import Foreign.Object (empty)
import MerchantConfig.Types (AppConfig)
import Foreign (Foreign)
import Foreign.Object (Object)
import Screens (ScreenName(..), getScreen)

initData :: HelpAndSupportScreenState
initData = {
  data: {
    source : "",
    destination : "",
    date : "",
    time : "",
    rating : 0,
    driverName : "",
    totalAmount : "",
    isNull : true,
    isLoading: true,
    status : "",
    rideStatus : "",
    rideCreatedAt : "",
    rideStartTime : "",
    rideStartTimeUTC : "",
    rideEndTimeUTC : "",
    rideEndTime : "",
    vehicleNumber : "",
    fromScreen : getScreen HOME_SCREEN,
    rideId : "",
    tripId : "",
    bookingId : "",
    faresList : [],
    email : "",
    description : "",
    accountStatus : ACTIVE,
    config : getAppConfig appConfig,
    vehicleVariant : Nothing,
    logField : empty,
    issueList : [],
    resolvedIssueList : [],
    ongoingIssueList : [],
    issueListType : HELP_AND_SUPPORT_SCREEN_MODAL,
    categories : [],
    merchantExoPhone : "",
    isFaqListEmpty : true
  },
  props:{
    apiFailure : false
  , isCallConfirmation : false
  , showDeleteAccountView : false
  , btnActive : false
  , needIssueListApiCall : true
  }

}

-- ################################################  Types   ################################################

type HelpAndSupportScreenState =
  {
    data :: HelpAndSupportScreenData,
    props :: HelpAndSuportScreenProps
  }

type HelpAndSupportScreenData =
  {
    date :: String,
    time :: String,
    rating :: Int,
    source :: String,
    destination :: String,
    driverName :: String,
    totalAmount :: String,
    isNull :: Boolean,
    isLoading :: Boolean,
    faresList :: Array FareComponent,
    fromScreen :: String,
    status :: String,
    rideStatus :: String,
    rideCreatedAt :: String,
    rideStartTime :: String,
    rideStartTimeUTC :: String,
    rideEndTimeUTC :: String,
    rideEndTime :: String,
    rideId :: String,
    vehicleNumber :: String,
    tripId :: String,
    bookingId :: String,
    email :: String,
    description :: String,
    accountStatus :: DeleteStatus ,
    config :: AppConfig,
    vehicleVariant :: Maybe VehicleVariant,
    issueList :: Array IssueInfo,
    ongoingIssueList :: Array IssueInfo,
    resolvedIssueList :: Array IssueInfo,
    issueListType :: IssueModalType,
    categories :: Array CategoryListType,
    merchantExoPhone :: String,
    logField :: Object Foreign,
    isFaqListEmpty :: Boolean
  }

type HelpAndSuportScreenProps =
  {
    apiFailure :: Boolean
  , isCallConfirmation :: Boolean
  , showDeleteAccountView :: Boolean
  , btnActive :: Boolean
  , needIssueListApiCall :: Boolean
  }