{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.ReferralScreen.ScreenData where

import Screens.Types (ReferralScreenState, BottomNavBarState, ReferralType(..), LeaderBoardType(..), RankCardData)
import PrestoDOM.Types.Core (toPropValue)
import Data.Maybe (Maybe(..))
import Foreign.Object (empty)
import ConfigProvider
import Common.Types.App (CalendarDate, CalendarWeek)

initData :: ReferralScreenState
initData =
  { data:
      { referralCode: ""
      , confirmReferralCode: ""
      , password: ""
      , driverInfo:
          { driverName: ""
          , driverMobile: Just ""
          , vehicleRegNumber: ""
          , referralCode: Nothing
          , vehicleVariant: ""
          }
      , driverPerformance:
          { referrals:
              { totalActivatedCustomers: 0
              , totalReferredCustomers: 0
              }
          }
      , logField: empty
      , config: getAppConfig appConfig
      }
  , props:
      { primarybtnActive: false
      , confirmBtnActive: false
      , passwordPopUpVisible: false
      , callSupportPopUpVisible: false
      , enableReferralFlowCount: 0
      , stage: LeaderBoard
      , seconds: 4
      , id: "SuccessScreenTimerId"
      , firstTime: false
      , leaderBoardType: Daily
      , showDateSelector: false
      , days: []
      , weeks: []
      , selectedDay: dummyDay
      , selectedWeek: dummyWeek
      , rankersData: []
      , currentDriverData: dummyCurrentDriverData
      , showShimmer: true
      , noData: false
      , lastUpdatedAt: ""
      }
  }

dummyRankData :: RankCardData
dummyRankData = { goodName: "-", profileUrl: Nothing, rank: 0, rides: 0, gender: "UNKNOWN" }

dummyCurrentDriverData :: RankCardData
dummyCurrentDriverData = { goodName: "Driver", profileUrl: Nothing, rank: 8, rides: 155, gender: "UNKNOWN" }

dummyDay :: CalendarDate
dummyDay =
  { date: 0
  , utcDate: ""
  , month: ""
  , year: 0
  }

dummyWeek :: CalendarWeek
dummyWeek =
  { startDate: 0
  , utcStartDate: ""
  , endDate: 0
  , utcEndDate: ""
  , startMonth: ""
  , endMonth: ""
  }
