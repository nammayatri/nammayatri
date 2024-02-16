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
import Engineering.Helpers.Commons as EHC
import MerchantConfig.DefaultConfig as DC
import RemoteConfig as RC
import Prelude ((==))

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
      , emergencyContactsList: []
      , removedContactDetail:
          { isSelected: false
          , name: ""
          , number: ""
          , enableForFollowing: false
          , priority : 1
          }
      , currentLocation : "Loading..."
      , vehicleDetails : "Loading..."
      , videoList : []
      , sosType : Nothing
      }
  , props:
      { onRide: false
      , setupStage: SetDefaultEmergencyContacts
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
      }
  }

defaultTimerValue :: Int
defaultTimerValue = if EHC.os == "IOS" then 5 else 6