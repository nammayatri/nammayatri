{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyFlow.ScreenData where

import Data.Maybe (Maybe(..))
import Screens.Types 
import Engineering.Helpers.Commons as EHC
import MerchantConfig.DefaultConfig as DC
import Services.API as API
import Prelude ((==))
import ConfigProvider (getAppConfig, appConfig)

initData :: NammaSafetyScreenState
initData =
  { data:
      { shareToEmergencyContacts: false
      , nightSafetyChecks: false
      , hasCompletedMockSafetyDrill : false
      , shareTripWithEmergencyContactOption : API.NEVER_SHARE
      , shareOptionCurrent : API.NEVER_SHARE
      , hasCompletedSafetySetup: false
      , sosId: ""
      , rideId: ""
      , updateActionType: ""
      , videoPath: ""
      , emergencyContactsList: []
      , removedContactDetail:
          { isSelected: false
          , name: ""
          , number: ""
          , enableForFollowing: false
          , enableForShareRide: false
          , onRide : false
          , priority : 1
          }
      , currentLocation : "Loading..."
      , vehicleDetails : "Loading..."
      , videoList : []
      , sosType : Nothing
      , config : getAppConfig appConfig
      , lastRideDetails : Nothing
      }
  , props:
      { setupStage: SetDefaultEmergencyContacts
      , timerId: ""
      , timerValue: defaultTimerValue
      , recordingState: NOT_RECORDING
      , triggeringSos: false
      , confirmPopup: false
      , showInfoPopUp: false
      , enableLocalPoliceSupport: false
      , localPoliceNumber: ""
      , showShimmer : true
      , showTestDrill : false
      , confirmTestDrill : false
      , educationViewIndex : Nothing
      , showCallPolice : false
      , shouldCallAutomatically : false
      , fromDeepLink : false
      , showRideShareOptionsPopup : false
      , showPastRidePopUp : false
      , showVideoView : false
      , isSafetyCenterDisabled : false
      , fromBannerLink : false
      , checkPastRide : false
      , reportPastRide : false
      }
  }

defaultTimerValue :: Int
defaultTimerValue = if EHC.os == "IOS" then 5 else 6