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
import Data.Array (length) as DA
import Data.Array ((!!), null, filter, elem)
import Language.Types (STR(..))
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Log (trackAppActionClick)
import PrestoDOM (Eval, update, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types (FaqScreenState)
import Services.API (RideBookingAPIDetails(..), RideBookingListRes(..), RideBookingDetails(..))
import Data.String
import Common.Types.App (LazyCheck(..), CategoryListType, FaqCardDropDownInfo, FaqStringType)
import Effect.Unsafe (unsafePerformEffect)
import Language.Strings (getString)
import Data.Function.Uncurried (runFn2)
import Locale.Utils
import Engineering.Helpers.BackTrack (getState)
import Types.App (GlobalState(..))
import Services.Backend as Remote
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
          , allowedRideStatuses: Nothing
          }
        ]


dropDownStringTransform :: String -> FaqStringType
dropDownStringTransform str =
  let 
    newStrings = DS.split(DS.Pattern ("{!!!}")) str
    value' = fromMaybe "" (newStrings DA.!! 1)
    type' = fromMaybe "" (newStrings DA.!! 0)
  in 
    {value: value', messageStringType: type' }

dropDownStringTypes :: String -> Array FaqStringType
dropDownStringTypes str = 
  let
    newStrings = DS.split(DS.Pattern ("{SUBPART}")) str
  in 
    map dropDownStringTransform newStrings

dropDownImageSplit :: String -> Array String
dropDownImageSplit str = DS.split(DS.Pattern ("{IMAGE}")) str