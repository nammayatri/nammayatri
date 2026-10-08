{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.DataExplainWithFetch.ScreenData where

import Screens.Types
import ConfigProvider (getAppConfig, appConfig)
import Data.Array as Array
import Data.Function.Uncurried (runFn3)
import Data.Maybe (fromMaybe, Maybe(..))
import DecodeUtil (getAnyFromWindow)
import Engineering.Helpers.Commons as EHC
import Language.Strings (getString, getStringWithoutNewLine)
import Language.Types (STR(..))
import MerchantConfig.DefaultConfig as DC
import Prelude ((==), ($))
import Screens.EmergencyContactsScreen.ScreenData (neverShareRideOption, alwaysShareRideOption, shareWithTimeContraintsRideOption)
import Services.API as API

initData :: DataFetchScreenState
initData =
  let
    appConfig' = getAppConfig appConfig
  in
    { data:
        { bannerData:
            { bannerItem: Nothing
            , currentBanner: 0
            , bannerScrollState: "0"
            , currentPage: 0
            }
        , headerValue: getHeaderValue $ SafetyCheckIn []
        , emergencyContactsList: []
        , unExpectedEventChecks: API.NEVER_SHARE
        , postRideCheck: API.NEVER_SHARE
        , notifySafetyTeam: false
        , emergencySOSShake: false
        , hasCompletedMockSafetyDrill: false
        , autoCallDefaultContact: false
        , informPoliceSos: false
        , notifySosWithEmergencyContacts: false
        , defaultSelectedContact:
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
        }
    , props:
        { showLoader: false
        , showDropDown: false
        , dropDownAction: ""
        , stageSetUpCompleted: false
        }
    , config:
        { appConfig: appConfig'
        , stage: stageData $ SafetyCheckIn []
        , stageSteps: getStepsNumber $ SafetyCheckIn []
        , currentStep: 0
        }
    }

stageData :: NammaSafetyStage -> NammaSafetyStage
stageData stage = case stage of
  TrustedContacts _ ->
    TrustedContacts
      [ { dynamicViewData:
            [ Title titleConfig { titleText = getString APP_CALL_CHAT }
            , SubTitle subTitleConfig { subTitleText = getString TRUSTED_CONTACT_DESC }
            , CheckBoxSelection checkBoxSelectionConfig { title = getString ENABLE_LIVE_TRACKING }
            ]
        , imageUrl: "ny_ic_share_explain"
        , primaryButtonText: getString DONE
        , primaryButtonAction: "UpdateEmergencyContacts"
        }
      ]
  SafetyCheckIn _ ->
    SafetyCheckIn
      [ { dynamicViewData:
            [ Title titleConfig { titleText = getString UNEXPECTED_EVENT_CHECK }
            , ImageComponent imageComponentConfig { imageUrl = "ny_ic_everything_rounded" }
            , BoxContainer
                boxContainerConfig
                  { title = getString UNEXPECTED_EVENT_CHECK
                  , subTitle = getString UNEXPECTED_EVENT_CHECK_DESC
                  , noteText = ""
                  , noteImageIcon = ""
                  }
            , DropDownWithHeader
                dropDownWithHeaderConfig
                  { headerText = getString UNEXPECTED_EVENT_CHECK_TIMINGS
                  , currentValue = shareWithTimeContraintsRideOption
                  , dropDownItems = [ alwaysShareRideOption, shareWithTimeContraintsRideOption ]
                  }
            ]
        , imageUrl: "ny_ic_everything_okay"
        , primaryButtonText: getString NEXT
        , primaryButtonAction: "UpdateUnExpectedEventChecks"
        }
      , { dynamicViewData:
            [ Title titleConfig { titleText = getString POST_RIDE_CHECK }
            , ImageComponent imageComponentConfig { imageUrl = "ny_ic_safe_journey_rounded" }
            , BoxContainer
                boxContainerConfig
                  { title = getString POST_RIDE_CHECK
                  , subTitle = getString POST_RIDE_CHECK_DESC
                  }
            , DropDownWithHeader
                dropDownWithHeaderConfig
                  { headerText = getString POST_RIDE_CHECK_TIMINGS
                  , currentValue = shareWithTimeContraintsRideOption
                  , dropDownItems = [ alwaysShareRideOption, shareWithTimeContraintsRideOption ]
                  }
            ]
        , imageUrl: "ny_ic_safe_journey_post_ride"
        , primaryButtonText: getString NEXT
        , primaryButtonAction: "PostRideCheck"
        }
      -- REMOVED FOR NOW
      --, { dynamicViewData:
      --       [ Title titleConfig { titleText = getString SAFETY_TEAM_NOTIFICATION }
      --       , ImageComponent imageComponentConfig { imageUrl = "ny_ic_no_resp_checkin_rounded" }
      --       , BoxContainer
      --           boxContainerConfig
      --             { title = getString NOTIFY_SAFETY_TEAM
      --             , subTitle = getString NOTIFY_SAFETY_TEAM_SUB
      --             , noteText = getString NOTIFY_SAFETY_TEAM_NOTE
      --             , noteImageIcon = "ny_ic_police_siren"
      --             }
      --       ]
      --   , imageUrl: "ny_ic_safety_team_checkin"
      --   , primaryButtonText: getString DONE
      --   , primaryButtonAction: "NotifySafetyTeam"
      --   }
      ]
  EmergencyActions _ -> do
    let appName = fromMaybe (getAppConfig appConfig).appData.name $ runFn3 getAnyFromWindow "appName" Nothing Just
    EmergencyActions
      [ { dynamicViewData:
            [ Title
                titleConfig
                  { titleText = getString EMERGENCY_SOS_NEW
                  }
            , ImageComponent imageComponentConfig { imageUrl = "ny_ic_rounded_sos_button" }
            , SubTitle
                subTitleConfig
                  { subTitleText = getString EMERGENCY_SOS_SUB
                  }
            , BoxContainer boxContainerConfig { title = getString SHAKE_TO_ACTIVATE, subTitle = getString $ SHAKE_TO_ACTIVATE_SUB appName}
            ]
        , imageUrl: "ny_ic_emergency_sos_banner"
        , primaryButtonText: getString NEXT
        , primaryButtonAction: "EmergencySOSShake"
        }
      , { dynamicViewData:
            [ Title titleConfig { titleText = getString AUTOMATIC_CALL_SOS }
            , ImageComponent imageComponentConfig { imageUrl = "ny_ic_trusted_contact_rounded" }
            , SubTitle subTitleConfig { subTitleText = getString AUTOMATIC_CALL_SOS_SUB }
            , BoxContainer boxContainerConfig { title = getString PLACE_DEFAULT_CALL, subTitle = "" }
            , SubTitle subTitleConfig { subTitleText = getString DEFAULT_CALL_CONTACT }
            , NoteBox
                noteBoxConfig
                  { noteText = getString DEFAULT_CONTACT
                  , noteSubTitle = getString DEFAULT_CONTACT_DESC
                  , noteImageIcon = "ny_ic_safety_headphone"
                  }
            ] -- TODO:: Need to add one more component
        , imageUrl: "ny_ic_default_contact"
        , primaryButtonText: getString NEXT
        , primaryButtonAction: "AutomaticCallOnEmergency"
        }
      , { dynamicViewData:
            [ Title
                titleConfig { titleText = getString MORE_EMERGENCY_ACTIONS }
            , ImageComponent imageComponentConfig { imageUrl = "ny_ic_emergency_actions_rounded" }
            , NoteBox
                noteBoxConfig
                  { noteText = getStringWithoutNewLine CALL_POLICE
                  , noteSubTitle = getString CALL_POLICE_DESC
                  , noteImageIcon = "ny_ic_police_siren"
                  }
            , NoteBox
                noteBoxConfig
                  { noteText = getStringWithoutNewLine RECORD_AUDIO
                  , noteSubTitle = getString RECORD_AUDIO_DESC
                  , noteImageIcon = "ny_ic_safety_microphone"
                  }
            , NoteBox
                noteBoxConfig
                  { noteText = getString SIREN
                  , noteSubTitle = getString SIREN_DESC
                  , noteImageIcon = "ny_ic_volume_safety"
                  }
            , NoteBox noteBoxConfig { noteText = getStringWithoutNewLine CALL_SAFETY_TEAM, noteSubTitle = getString CALL_SAFETY_TEAM_DESC, noteImageIcon = "ny_ic_safety_headphone" }
            ]
        , imageUrl: "ny_ic_emergency_actions_4"
        , primaryButtonText: getString DONE
        , primaryButtonAction: ""
        }
      ]
  SafetyDrill _ ->
    SafetyDrill
      [ { dynamicViewData:
            [ SubTitle
                subTitleConfig
                  { subTitleText = getString SAFETY_DRILL_DESC
                  }
            , SubTitle subTitleConfig { subTitleText = getString SAFETY_DRILL_SUB }
            , NoteBox noteBoxConfig { noteText = getString SAFETY_DRILL_NOTE }
            ]
        , imageUrl: "ny_ic_safety_drill_setup"
        , primaryButtonText: getString START_TEST_DRILL
        , primaryButtonAction: "SafetyTestDrill"
        }
      ]
  TrustedContactsActions _ ->
    TrustedContactsActions
      [ { dynamicViewData:
            [ Title titleConfig { titleText = getString RIDE_ACTIONS }
            , SubTitle subTitleConfig { subTitleText = getString RIDE_ACTIONS_SUB }
            , NoteBox
                noteBoxConfig
                  { noteText = getString LIVE_TRACKING
                  , noteSubTitle = getString LIVE_TRACKING_SUB
                  , noteImageIcon = "ny_ic_pin_locate_safety"
                  }
            , NoteBox
                noteBoxConfig
                  { noteText = getString CHAT_WITH_RIDER
                  , noteSubTitle = getString CHAT_WITH_RIDER_SUB
                  , noteImageIcon = "ny_ic_chat_message_safety"
                  }
            , Title titleConfig { titleText = getString EMERGENCY_ACTIONS }
            , SubTitle subTitleConfig { subTitleText = getString EMERGENCY_ACTIONS_SUB }
            , NoteBox
                noteBoxConfig
                  { noteText = getStringWithoutNewLine CALL_POLICE
                  , noteSubTitle = getString CALL_POLICE_DESC
                  , noteImageIcon = "ny_ic_police_siren"
                  }
            , NoteBox
                noteBoxConfig
                  { noteText = getStringWithoutNewLine CALL_SAFETY_TEAM
                  , noteSubTitle = getString CALL_SAFETY_TEAM_DESC
                  , noteImageIcon = "ny_ic_safety_headphone"
                  }
            ]
        , imageUrl: "ny_ic_trusted_contact_more"
        , primaryButtonText: getString DONE
        , primaryButtonAction: "UpdateTrustedContactsActions"
        }
      ]
  DriverSafetyStandards _ ->
    DriverSafetyStandards
      [ { dynamicViewData:
            [ Title titleConfig { titleText = getString CURRENT_INITIATIVES }
            , SubTitle subTitleConfig { subTitleText = getString CURRENT_INITIATIVES_SUB }
            , NoteBox
                noteBoxConfig
                  { noteText = getString DRIVER_VERIFICATION
                  , noteImageIcon = "ny_ic_user_profile_check"
                  , noteSubTitle = getString DRIVER_VERIFICATION_SUB
                  }
            , NoteBox
                noteBoxConfig
                  { noteText = getString SAFETY_FEEDBACK 
                  , noteImageIcon = "ny_ic_chat_message_safety"
                  , noteSubTitle = getString SAFETY_FEEDBACK_SUB
                  }
            , NoteBox
                noteBoxConfig
                  { noteText = getString SAFETY_TRAINING
                  , noteImageIcon = "ny_ic_graduation_hat"
                  , noteSubTitle = getString SAFETY_TRAINING_SUB
                  }
            , NoteBox
                noteBoxConfig
                  { noteText = getString DRIVER_ID_CHECK
                  , noteImageIcon = "ny_ic_user_info_details"
                  , noteSubTitle = getString DRIVER_ID_CHECK_SUB
                  }
            , NoteBox
                noteBoxConfig
                  { noteText = getString DATA_PRIVACY
                  , noteImageIcon = "ny_ic_data_safety"
                  , noteSubTitle = getString DATA_PRIVACY_SUB
                  }
            , Title titleConfig { titleText = getString UPCOMING_INITIATIVES }
            , SubTitle subTitleConfig { subTitleText = getString UPCOMING_INITIATIVES_DESC }
            , NoteBox
                noteBoxConfig
                  { noteText = getString FAVOURITE_DRIVER
                  , noteImageIcon = "ny_ic_driver_fev_safety"
                  , noteSubTitle = getString FAVOURITE_DRIVER_SUB
                  }
            , NoteBox
                noteBoxConfig
                  { noteText = getString WOMEN_DRIVERS
                  , noteImageIcon = "ny_ic_women"
                  , noteSubTitle = getString WOMEN_DRIVERS_SUB
                  }
            , NoteBox
                noteBoxConfig
                  { noteText = getString DASHCAM
                  , noteImageIcon = "ny_ic_safety_video"
                  , noteSubTitle = getString DASHCAM_SUB
                  }
            ]
        , imageUrl: "ny_ic_safety_explore_features"
        , primaryButtonText: getString DONE
        , primaryButtonAction: "UpdateDriverSafetyStandards"
        }
      ]

boxContainerConfig :: BoxContainerConfig
boxContainerConfig =
  { title: ""
  , subTitle: ""
  , toggleButton: true
  , noteText: ""
  , noteImageIcon: ""
  }

imageComponentConfig :: ImageComponentConfig
imageComponentConfig =
  { imageUrl: ""
  }

dropDownWithHeaderConfig :: DropDownWithHeaderConfig
dropDownWithHeaderConfig =
  { headerText: ""
  , currentValue: { key: API.NEVER_SHARE, value: "" }
  , dropDownItems: []
  }

noteBoxConfig :: NoteBoxConfig
noteBoxConfig =
  { noteText: ""
  , noteImageIcon: ""
  , noteSubTitle: ""
  }

titleConfig :: TitleConfig
titleConfig =
  { titleText: ""
  }

subTitleConfig :: SubTitleConfig
subTitleConfig =
  { subTitleText: ""
  }

checkBoxSelectionConfig :: CheckBoxSelectionConfig
checkBoxSelectionConfig =
  { title: ""
  , contacts: []
  , selectedContact:
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
  }

getStepsNumber :: NammaSafetyStage -> Int
getStepsNumber stage = case stage of
  TrustedContacts items -> Array.length items
  SafetyCheckIn items -> Array.length items
  EmergencyActions items -> Array.length items
  SafetyDrill items -> Array.length items
  TrustedContactsActions items -> Array.length items
  DriverSafetyStandards items -> Array.length items

getHeaderValue :: NammaSafetyStage -> String
getHeaderValue stage = case stage of
  TrustedContacts _ -> "Trusted Contacts"
  SafetyCheckIn _ -> "Safety Check Ins"
  EmergencyActions _ -> "Emergency Actions"
  SafetyDrill _ -> "Safety Test Drill"
  TrustedContactsActions _ -> "Trusted Contacts Actions"
  DriverSafetyStandards _ -> "Driver Safety Standards"
