{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyFlow.ScreenData where

import Data.Maybe
import Screens.Types

import MerchantConfig.DefaultConfig as DC
import RemoteConfigs as RC

initData :: NammaSafetyScreenState
initData =
  { data:
      { shareToEmergencyContacts: false
      , nightSafetyChecks: false
      , hasCompletedMockSafetyDrill : false
      , shareTripWithEmergencyContacts : false
      , hasCompletedSafetySetup: false
      , sosId: ""
      , rideId: ""
      , updateActionType: ""
      , videoPath: ""
      , contactsList: []
      , removedContactDetail:
          { isSelected: false
          , name: ""
          , number: ""
          , enableForFollowing: false
          , priority : 1
          }
      , currentLocation : "Loading..."
      , vehicleDetails : "Loading..."
      , videoList : educationData --RC.safetyVideoConfig
      }
  , props:
      { onRide: false
      , setupStage: SetDefaultEmergencyContacts
      , timerId: ""
      , timerValue: 6
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
      , setYoutubeView : true
      , showCallPolice : false
      , shouldCallAutomatically : false
      }
  }

educationData :: Array RC.SafetyVideoConfig
educationData =
  [ { title: "NAMMA_SAFETY_MEASURES", videoId: "u57l5jwRvLE", coverImageUrl: "ny_ic_namma_safety_measures" }
  , { title: "SAFETY_GUIDELINES_FOR_YOU", videoId: "N4BpjB3jkjE", coverImageUrl: "ny_ic_namma_safety_guidlines" }
  , { title: "ABOUT_SOS", videoId: "_vFXj8PlraM", coverImageUrl: "ny_ic_about_sos_icon" }
  ]
