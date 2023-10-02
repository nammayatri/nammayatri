{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DriverEarningsScreen.ScreenData where

import Foreign.Object (empty)
import Prelude ((-), negate)
import Resource.Constants (tripDatesCount)
import Screens.Types (AnimationState(..), DriverEarningsScreenState, DriverEarningsSubView(..))

initData :: DriverEarningsScreenState
initData = {
  data : {
    coinHistoryItems : [{
      event : "Ride COMPLETED",
      timestamp : "31/5/2022 7:45pm",
      coins : 1
    },{
      event : "Ride CANCELLED",
      timestamp : "31/5/2022 7:45pm",
      coins : -2
    }, {
      event : "Ride COMPLETED",
      timestamp : "31/5/2022 7:45pm",
      coins : 1
    }, {
      event : "Ride CANCELLED",
      timestamp : "31/5/2022 7:45pm",
      coins : -2
    }
    ],
    usageHistoryItems : [],
    planItems : [
      {
        name : "Daily Unlimited Plan",
        coins : 100
      },
      {
        name : "Weekly Unlimited Plan",
        coins : 600
      }
    ]
  }
  , props : {
    subView : YATRI_COINS_VIEW,
    selectedPlanIndex : 0,
    selectedPlanQuantity : 0
  }
}