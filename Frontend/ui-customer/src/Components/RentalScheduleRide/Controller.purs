{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.RentalScheduleRide.Controller where

import Components.LocationListItem as LocationListItem
import Components.LocationTagBar as LocationTagBarController
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryButton.Controller as PrimaryButtonController
import Data.Maybe (Maybe(..))
import Prelude (show)
import PrestoDOM (Visibility(..))
import Screens.Types (RentalStage(..), SearchLocationModelType, LocationListItemState, LocItemType(..))
import MerchantConfig.Types (AppConfig)
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import Common.Types.App (LazyCheck(..))
import Prelude ((<>))
import Foreign.Object (Object)
import Foreign (Foreign)

data Action = GoBack
            | PrimaryButtonActionController PrimaryButtonController.Action

type RentalScheduleState = {
    isCancelled :: Boolean
  , price :: String
  , address :: String
  , baseDuration :: String
  , baseDistance :: String
  , primaryButton :: PrimaryButtonConfig
}

type PrimaryButtonConfig = {
    text :: String
  , background :: String
  , height :: Int
  , cornerRadius :: Number
}

config :: RentalScheduleState
config = { 
    isCancelled : false
  , price : "420"
  , address : ""
  , baseDuration : "8"
  , baseDistance : "80"
  , primaryButton : {
        text : "Okay"
      , background : "#2C2F3A"
      , height : 60
      , cornerRadius : 8.0
    }
  }
