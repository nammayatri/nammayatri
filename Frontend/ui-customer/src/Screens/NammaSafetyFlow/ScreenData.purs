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
import Prelude ((==), ($))
import ConfigProvider (getAppConfig, appConfig)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Function.Uncurried (runFn3)
import DecodeUtil (getAnyFromWindow)
import Language.Types (STR(..))
import Screens.EmergencyContactsScreen.ScreenData (neverShareRideOption)
import Common.Types.App as CTA
import Screens (ScreenName(..), getScreen)

initData :: NammaSafetyScreenState
initData =
  let
    config = getAppConfig appConfig
  in
    { data:
        { shareToEmergencyContacts: false
        , settingsAPIResponse: invalidSettingsAPIResponse
        , nightSafetyChecks: false
        , hasCompletedMockSafetyDrill: false
        , shareTripWithEmergencyContactOption: API.NEVER_SHARE
        , shareOptionCurrent: API.NEVER_SHARE
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
            , shareTripWithEmergencyContactOption: neverShareRideOption
            , onRide: false
            , priority: 1
            , contactPersonId: Nothing
            , isFollowing: Nothing
            , notifiedViaFCM : Nothing
            }
        , currentLocation: "Loading..."
        , currentLatLon : Nothing
        , vehicleDetails: "Loading..."
        , videoList: []
        , sosType: Nothing
        , config: config
        , lastRideDetails: Nothing
        , bannerData:
            { bannerItem: Nothing
            , currentBanner: 0
            , bannerScrollState: "0"
            , currentPage: 0
            }
        , extraSafetyExplaination: [
          { title: TRUSTED_CONTACT_HELP
              , prefixImage: "ny_ic_trusted_contacts"
              , prefixImageCompleted: "ny_ic_trusted_contacts"
              , stepNumber: "5"
              , isCompleted: false
              , labelText: Nothing
              , navigation: TrustedContactsActions []
              },
              { title: DRIVER_SAFETY_STANDARDS
              , prefixImage: "ny_ic_profile_shine"
              , prefixImageCompleted: "ny_ic_profile_shine"
              , stepNumber: "6"
              , isCompleted: false
              , labelText: Nothing
              , navigation: DriverSafetyStandards []
              }
        ]
        , safetySetupSteps:
            [ { title: TRUSTED_CONTACT
              , prefixImage: "ny_ic_trusted_contacts"
              , prefixImageCompleted: "ny_ic_trusted_contacts_completed"
              , stepNumber: "1"
              , isCompleted: false -- EmergencyContacts > 0
              , labelText: Just TRUSTED_CONTACT_HIGHLIGHT
              , navigation: TrustedContacts []
              }
            , { title: SAFETY_CHECK_IN
              , prefixImage: "ny_ic_safety_checkin"
              , prefixImageCompleted: "ny_ic_safety_checkin_competed"
              , stepNumber: "2"
              , isCompleted: false -- UnExpected/ PostRide/ SafetyTeam/ Police
              , labelText: Nothing
              , navigation: SafetyCheckIn []
              }
            , { title: EMERGENCY_ACTIONS
              , prefixImage: "ny_ic_emergency_actions"
              , prefixImageCompleted: "ny_ic_emergency_actions_completed"
              , stepNumber: "3"
              , isCompleted: false -- shakeToSOS/ autoCallToEmergency
              , labelText: Nothing
              , navigation: EmergencyActions []
              }
            , { title: SAFETY_DRILL
              , prefixImage: "ny_ic_safety_drill_steps"
              , prefixImageCompleted: "ny_ic_safety_drill_completed"
              , stepNumber: "4"
              , isCompleted: false -- safetyDrillCompleted
              , labelText: Nothing
              , navigation: SafetyDrill []
              }
            ]
          , autoCallDefaultContact : false
        }
    , props:
        { setupStage: SetDefaultEmergencyContacts
        , timerId: ""
        , timerValue: defaultTimerValue
        , triggeringSos: false
        , confirmPopup: false
        , showInfoPopUp: false
        , enableLocalPoliceSupport: false
        , localPoliceNumber: ""
        , showShimmer: true
        , isFromSafetyCenter : false
        , showTestDrill: false
        , confirmTestDrill: false
        , educationViewIndex: Nothing
        , showCallPolice: false
        , shouldCallAutomatically: false
        , fromDeepLink: false
        , showRideShareOptionsPopup: false
        , showPastRidePopUp: false
        , showVideoView: false
        , isSafetyCenterDisabled: false
        , fromBannerLink: false
        , checkPastRide: false
        , reportPastRide: false
        , appName: fromMaybe config.appData.name $ runFn3 getAnyFromWindow "appName" Nothing Just
        , isOffUs: false
        , triggerSiren : false
        , isAudioRecordingActive : false
        , audioRecordingStatus : CTA.NOT_RECORDING
        , recordingTimer : "00 : 00"
        , recordingTimerId : ""
        , recordedAudioUrl : Nothing
        , showMenu : false
        , policeCallTimerValue : 5
        , policeCallTimerId : ""
        , defaultCallPopup : false
        , fromScreen : Nothing
        }
    }

defaultTimerValue :: Int
defaultTimerValue = if EHC.os == "IOS" then 5 else 6

invalidSettingsAPIResponse :: API.GetEmergencySettingsRes
invalidSettingsAPIResponse =
  API.GetEmergencySettingsRes
    { nightSafetyChecks: false
    , hasCompletedSafetySetup: false
    , defaultEmergencyNumbers: []
    , enablePoliceSupport: false
    , localPoliceNumber: Nothing
    , hasCompletedMockSafetyDrill: false
    , shareTripWithEmergencyContacts: Nothing
    , shareTripWithEmergencyContactOption: Nothing
    , autoCallDefaultContact: false
    , enableOtpLessRide: Nothing
    , enablePostRideSafetyCheck: API.NEVER_SHARE
    , enableUnexpectedEventsCheck: API.NEVER_SHARE
    , informPoliceSos: false
    , notifySafetyTeamForSafetyCheckFailure: false
    , notifySosWithEmergencyContacts: false
    , shakeToActivate: false
    , safetyCheckStartTime : Nothing
    , safetyCheckEndTime : Nothing
    }
