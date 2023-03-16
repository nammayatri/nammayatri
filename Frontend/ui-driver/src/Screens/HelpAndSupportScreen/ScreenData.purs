{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HelpAndSupportScreen.ScreenData where

import Screens.Types(HelpAndSupportScreenState)
import Prelude (class Eq)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep (class Generic)


initData :: HelpAndSupportScreenState
initData = {
  data:  {mapImage : "",
          date : "",
          time : "",
          source : "",
          destination : "",
          fare : 0,
          tripId : "",
          customerName : "",
          coveredDistance : "",
          durationOfTrip : "",
          rating : 0
          },
  props: {isNoRides: false }
}


data ListOptions = GettingStartedFaq | OtherIssues | CallSupportCenter
derive instance genericListOptions :: Generic ListOptions _
instance eqListOptions :: Eq ListOptions where eq = genericEq


type Listtype =
    { icon :: String,
      menuOptions :: ListOptions
    }

optionList :: Array Listtype
optionList = 
    [
      {menuOptions: GettingStartedFaq , icon:"ny_ic_help_circle_transparent,https://assets.juspay.in/nammayatri/images/driver/ny_ic_help_circle_transparent.png"},
      {menuOptions: OtherIssues , icon:"ny_ic_clip_board,https://assets.juspay.in/nammayatri/images/common/ny_ic_clip_board.png"},
      {menuOptions: CallSupportCenter , icon:"ny_ic_head_phones,https://assets.juspay.in/nammayatri/images/driver/ny_ic_head_phones.png"}
    ]