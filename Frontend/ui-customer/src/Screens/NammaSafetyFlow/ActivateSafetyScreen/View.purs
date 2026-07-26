{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyFlow.ActivateSafetyScreen.View where

import Animation (screenAnimation, fadeIn)
import Mobility.Prelude (boolToInvisibility, boolToVisibility)
import Prelude
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Accessiblity(..), Visibility(..), afterRender, alignParentBottom, alpha, background, clickable, color, cornerRadius, gravity, height, imageView, imageWithFallback, linearLayout, margin, onBackPressed, onClick, orientation, padding, relativeLayout, rippleColor, scrollView, singleLine, stroke, text, textFromHtml, textView, visibility, weight, width, accessibilityHint, accessibility, fillViewport)
import Screens.NammaSafetyFlow.ComponentConfig
import Screens.NammaSafetyFlow.Components.HelperViews (emptyTextView, layoutWithWeight, safetyPartnerView, separatorView, shimmerView)
import Screens.Types (NammaSafetyScreenState, IndividualRideCardState)
import Timers (startTimer)
import Common.Types.App (LazyCheck(..))
import Components.PrimaryButton as PrimaryButton
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array (elem, mapWithIndex, null, length)
import Data.Maybe (Maybe(..), maybe, fromMaybe, isJust)
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons as EHC
import Font.Style as FontStyle
import Helpers.Utils (FetchImageFrom(..), fetchImage, getLocationName)
import Language.Strings (getString, getStringWithoutNewLine)
import Language.Types (STR(..))
import Presto.Core.Types.Language.Flow (doAff)
import Screens.NammaSafetyFlow.ActivateSafetyScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.NammaSafetyFlow.Components.ContactCircle as ContactCircle
import Screens.NammaSafetyFlow.Components.HeaderView as Header
import Services.API (GetSosDetailsRes(..), SosFlow(..))
import Services.Backend as Remote
import Styles.Colors as Color
import Types.App (defaultGlobalState)
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Components.SourceToDestination as SourceToDestination
import Data.Either (Either (..))
import PrestoDOM.Animation as PrestoAnim
import JBridge as JB
import Engineering.Helpers.Utils as EHU
import Screens.NammaSafetyFlow.Components.SafetyUtils (getVehicleDetails, getPrimaryContact)
import Data.String as DS
import Components.Safety.SosButtonAndDescription as SosButtonAndDescription
import Components.Safety.Utils as SU
import Components.Safety.SafetyActionTileView as SafetyActionTileView
import Components.OptionsMenu as OptionsMenu
import Components.Safety.SafetyAudioRecording as SafetyAudioRecording
import Effect.Uncurried (runEffectFn2)
import Components.PopupWithCheckbox.View as PopupWithCheckbox
import Data.Function.Uncurried (runFn2)

screen :: NammaSafetyScreenState -> Screen Action NammaSafetyScreenState ScreenOutput
screen initialState =
  { initialState
  , view: view
  , name: "ActivateSafetyScreen"
  , globalEvents:[ ( \push -> do
            _ <- pure $ runFn2 JB.storeOnPauseCallback push OnPauseCallback
            void $ launchAff $ EHC.flowRunner defaultGlobalState
              $ do
                  eiResponse <- Remote.getEmergencySettings ""
                  case eiResponse of
                    Right response -> do
                      EHC.liftFlow $ push $ UpdateEmergencySettings response
                      when (initialState.data.sosType /= Just Police)
                        $ do
                            if elem initialState.data.sosId ["", "mock-sos"] then do
                              eiSosResponse <- Remote.getSosDetails getRideId
                              case eiSosResponse of
                                Right (GetSosDetailsRes sosDetails) -> 
                                  case sosDetails.sos of
                                    Just sos -> do
                                      EHC.liftFlow $ push $ UpdateSosId sos
                                      pure unit
                                    Nothing -> pure unit
                                Left err -> do
                                  let errMessage = if err.code == 400 
                                                     then err.response.errorMessage 
                                                     else getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
                                  void $ pure $ EHU.showToast errMessage
                            else
                              EHC.liftFlow $ push $ GoToActiveSos
                      EHC.liftFlow $ push $ DisableShimmer
                      pure unit
                    Left err -> pure unit
            void $ runEffectFn2 JB.storeCallBackUploadMultiPartData push UploadMultiPartDataCallback
            pure $ pure unit
        )
      ]
  , eval:
      \action state -> do
        let
          _ = spy "ActivateSafetyScreen action " action
        let
          _ = spy "ActivateSafetyScreen state " state
        eval action state
  }
  where
    getRideId = case initialState.data.lastRideDetails of
                  Nothing -> initialState.data.rideId
                  Just ride -> ride.rideId

view :: forall w. (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
view push state =
  screenAnimation
    $ relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , onBackPressed push $ const $ BackPressed
        , background Color.black900
        , afterRender
            ( \_ -> do
                getLocationName push 9.9 9.9 "Current Location" SelectedCurrentLocation
                pure unit
            )
            (const NoAction)
        ]$
        [ linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , padding padding'
            , visibility $ boolToVisibility $ not state.props.showShimmer
            ]
            [ Header.view (push <<< SafetyHeaderAction) headerConfig
            , if state.props.showTestDrill then Header.testSafetyHeaderView (push <<< SafetyHeaderAction) else emptyTextView
            , if state.props.confirmTestDrill then confirmSafetyDrillView state push 
              else if showCallPolice then dialPoliceView state push
              else activateSafetyView state push
            ]
        , shimmerView state
        ]
        <> if state.props.showMenu then [menuOptionModal push state] else []
        <> if state.props.defaultCallPopup then [contactsPopupView push state] else []

  where
  padding' = if EHC.os == "IOS" then (Padding 0 EHC.safeMarginTop 0 (if EHC.safeMarginBottom == 0 && EHC.os == "IOS" then 16 else EHC.safeMarginBottom)) else (PaddingLeft 0)

  headerConfig =
    (Header.config Language)
      { learnMoreTitle = getString LEARN_ABOUT_NAMMA_SAFETY
      , showLearnMore = not state.props.showCallPolice
      , useLightColor = true
      , title = getStringWithoutNewLine if not state.props.showCallPolice then EMERGENCY else CALL_POLICE
      , headerVisiblity = boolToInvisibility $ not state.props.confirmTestDrill
      , showCrossImage = not state.props.showCallPolice
      , showOptions = not state.props.showCallPolice 
      }
  showCallPolice = state.props.showCallPolice || (state.props.isSafetyCenterDisabled && not state.props.showTestDrill)

activateSafetyView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
activateSafetyView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , weight 1.0
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , weight 1.0
        ]
        [ scrollView
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , fillViewport true
            ]
            [ linearLayout
                [ height MATCH_PARENT
                , width MATCH_PARENT
                , orientation VERTICAL
                ]
                [ SosButtonAndDescription.view (push <<< SosButtonAndDescriptionAction) $ sosButtonConfig state
                , if state.props.triggeringSos then
                    triggeringSosView state push
                  else
                    otherActionsView state push
                ]
            ]
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , visibility $ boolToVisibility $ state.props.triggeringSos
        ]
        [ separatorView Color.black700 $ MarginVertical 10 16
        , PrimaryButton.view (push <<< CancelSosTrigger) $ btnConfig {id = "cancelSosTriggerBtn"}
        ]
    ]
  where 
    btnConfig = dismissSoSButtonConfig state 

sosButtonConfig :: NammaSafetyScreenState -> SosButtonAndDescription.Config
sosButtonConfig state =
  {
    triggeringSos: state.props.triggeringSos,
    showTestDrill: state.props.showTestDrill,
    timerValue: state.props.timerValue,
    sosDescription: [ imageWithTextConfig {text' = getString RECEIVE_CALL_FROM_SAFETY_TEAM}
                    , imageWithTextConfig {text' = getString NOTIFY_ALL_EMERGENCY_CONTACTS}
                    ],
    primaryContactAndEdit: 
          case contactName, state.data.autoCallDefaultContact of
            Just name, true -> Just imageWithTextConfig {text' = getString CALL <> " : " <> name, visibility = isJust contactName, useFullWidth = false, useMargin = false}
            _,_ -> Just imageWithTextConfig {text' = getString DEFAULT_CONTACT_NOT_SET, visibility = true, useFullWidth = false, useMargin = false},
    buttonText: case state.props.triggeringSos, state.props.showTestDrill of
      true, _ -> show state.props.timerValue
      false, true -> getString TEST_SOS
      false, false -> getString SOS,
    descriptionText: getString $ case state.props.triggeringSos, state.props.showTestDrill of
            true, true -> TEST_SOS_ACTIVATING_IN
            true, false -> EMERGENCY_SOS_ACTIVATING
            false, true -> PRESS_TO_START_TEST_DRILL
            false, false -> PRESS_IN_CASE_OF_EMERGENCY,
    showSosButton: true,
    isDisabled : state.props.showTestDrill && state.props.isAudioRecordingActive,
    editContactText : getString $ if isJust contactName && state.data.autoCallDefaultContact then EDIT else SET_NOW
  }
  where
    imageWithTextConfig =
      { text': ""
      , isActive: true
      , textColor: Color.white900
      , useMargin: true
      , useFullWidth: true
      , usePadding: false
      , image: Just "carousel_dot_inactive"
      , visibility: true
      , textStyle: FontStyle.Tags
      , showBullet: true
      }

    contactName = (getPrimaryContact state) <#> _.name

emergencyContactsView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
emergencyContactsView state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , stroke $ "1," <> Color.black700
    , background Color.blackOpacity12
    , orientation VERTICAL
    , margin $ Margin 16 16 16 16
    , padding $ Padding 16 16 16 16
    , cornerRadius 12.0
    , visibility $ boolToVisibility $ not state.props.triggeringSos
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , visibility $ boolToVisibility $ not state.props.showTestDrill
        ]
        [ SU.measureView configDescOne
        , SU.measureView configDescTwo
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , visibility $ boolToVisibility $ null state.data.emergencyContactsList || not state.data.shareToEmergencyContacts
        , gravity CENTER
        ]
        [ SU.measureView configDescThree
        , textView
            $ [ text $ "+ " <> getString ADD_EMERGENCY_CONTACTS
              , color Color.blue800
              , margin $ MarginTop 16
              , onClick push $ const AddContacts
              ]
            <> FontStyle.body1 TypoGraphy
        ]
    , textView
        $ [ text $ getString $ EMERGENCY_CONTACTS_CAN_TAKE_ACTION state.props.appName
          , color Color.white900
          , visibility $ boolToVisibility $ state.props.showTestDrill && not (null state.data.emergencyContactsList)
          , gravity CENTER
          ]
        <> FontStyle.tags TypoGraphy
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding $ Padding 12 12 12 12
        , orientation VERTICAL
        , background Color.black900
        , margin $ MarginTop 16
        , cornerRadius 12.0
        , visibility $ boolToVisibility $ state.data.shareToEmergencyContacts && (not $ null state.data.emergencyContactsList)
        ]
        [ textView
            $ [ text $ getString SELECT_CONTACT_TO_CALL
              , color Color.black500
              ]
            <> FontStyle.tags TypoGraphy
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER_VERTICAL
            , margin $ MarginTop 12
            ]
            [ linearLayout
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , gravity LEFT
                ]
                ( mapWithIndex
                    ( \index item ->
                        ContactCircle.view (ContactCircle.getContactConfig item index true true) (push <<< ContactAction)
                    )
                    state.data.emergencyContactsList
                )
            , layoutWithWeight
            , textView
                $ [ textFromHtml $ "<u>" <> getString EDIT_ACTIONS <> "</u>"
                  , color Color.blue800
                  , padding $ Padding 16 6 16 6
                  , onClick push $ const AddContacts
                  ]
                <> FontStyle.body1 TypoGraphy
            ]
        ]
    , textView
        $ [ text $ getString INFORM_EMERGENCY_CONTACTS
          , color Color.black500
          , visibility $ boolToVisibility state.props.showTestDrill
          , margin $ MarginTop 16
          ]
        <> FontStyle.tags TypoGraphy
    ]
  where
  configDescOne =
    { text': getString $ SAFETY_TEAM_WILL_BE_ALERTED state.props.appName
    , isActive: state.data.shareToEmergencyContacts && (not $ null state.data.emergencyContactsList)
    , textColor: Color.white900
    , useMargin: false
    , useFullWidth: true
    , usePadding: false
    , image: Nothing
    , visibility: true
    , textStyle: FontStyle.Tags
    , showBullet: false
    }

  configDescTwo =
    { text': getString $ EMERGENCY_CONTACTS_CAN_TAKE_ACTION state.props.appName
    , isActive: true
    , textColor: Color.white900
    , useMargin: true
    , useFullWidth: true
    , usePadding: false
    , image: Nothing
    , visibility: state.data.shareToEmergencyContacts && (not $ null state.data.emergencyContactsList)
    , textStyle: FontStyle.Tags
    , showBullet: false
    }

  configDescThree =
    { text': getString SHARE_LOCATION_AND_RIDE_DETAILS_EMERGENCY_CONTACT
    , isActive: true
    , textColor: Color.white900
    , useMargin: true
    , useFullWidth: true
    , usePadding: false
    , image: Just "ny_ic_info_white"
    , visibility: true
    , textStyle: FontStyle.Tags
    , showBullet: false
    }

oldActionsView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
oldActionsView state push =
  linearLayout
    ( [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , margin $ Margin 16 16 16 16
      , cornerRadius 12.0
      ]
        <> if state.props.showTestDrill then
            [ alpha 0.5
            , clickable false
            ]
          else
            []
    )
    [ orSeparatorView state
    , textView
        $ [ text $ getString AVAILABLE_IN_REAL_EMERGENCY
          , color Color.white900
          , margin $ MarginTop 4
          , visibility $ boolToVisibility state.props.showTestDrill
          ]
        <> FontStyle.captions TypoGraphy
    , linearLayout
        ( [ height WRAP_CONTENT
          , width MATCH_PARENT
          , gravity CENTER_VERTICAL
          , margin $ MarginTop 20
          ]
            <> if state.props.showTestDrill then [] else [ onClick push $ const $ ShowPoliceView SafetyActionTileView.OnClick ]
        )
        [ SU.measureView configActionOne
        , layoutWithWeight
        , imageView
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_right_white"
            , height $ V 20
            , width $ V 20
            ]
        ]
    , separatorView Color.black700 $ MarginVertical 16 16
    , linearLayout
        ( [ height WRAP_CONTENT
          , width MATCH_PARENT
          , gravity CENTER_VERTICAL
          ]
            <> if state.props.showTestDrill then
                []
              else
                [ onClick push $ const $ if state.data.config.feature.enableCustomerSupportForSafety then CallSupport else ShowSafetyIssueView ]
        )
        [ SU.measureView
            $ if state.data.config.feature.enableCustomerSupportForSafety then
                configActionTwo'
              else
                configActionTwo
        , layoutWithWeight
        , imageView
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_right_white"
            , height $ V 20
            , width $ V 20
            ]
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , visibility $ boolToVisibility $ not state.props.showTestDrill
        ]
        [ separatorView Color.black700 $ MarginVertical 16 16
        , linearLayout
            ( [ height WRAP_CONTENT
              , width MATCH_PARENT
              , gravity CENTER_VERTICAL
              ]
                <> if state.props.showTestDrill then [] else [ onClick push $ const GoToTestDrill ]
            )
            [ SU.measureView configActionThree
            , layoutWithWeight
            , imageView
                [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_right_white"
                , height $ V 20
                , width $ V 20
                ]
            ]
        ]
    ]
  where
  imageWithTextConfig =
    { text': ""
    , isActive: true
    , textColor: Color.white900
    , useMargin: false
    , usePadding: false
    , useFullWidth: false
    , image: Nothing
    , visibility: true
    , textStyle: FontStyle.Tags
    , showBullet: false
    }
  configActionOne = imageWithTextConfig
    { text' = getString CALL_POLICE
    , image = Just "ny_ic_police"
    }

  configActionTwo = imageWithTextConfig
    { text' = getString REPORT_SAFETY_ISSUE
    , image = Just "ny_ic_issue_box"
    }

  configActionTwo' = imageWithTextConfig
    { text' = getString CALL_CUSTOMER_SUPPORT
    , image = Just "ny_ic_support_unfilled"
    }

  configActionThree = imageWithTextConfig
    { text' = getString START_TEST_DRILL
    , image = Just "ny_ic_police"
    , visibility = not state.props.showTestDrill
    }

otherActionsView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
otherActionsView state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , cornerRadius 12.0
    , margin $ MarginVertical 16 16
    ]
    [ orSeparatorView state
    , relativeLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      ][ linearLayout
          [ visibility $ boolToInvisibility state.props.isAudioRecordingActive
          , height WRAP_CONTENT
          , width MATCH_PARENT
          ][ SafetyAudioRecording.view (push <<< SafetyAudioRecordingAction) $ safetyAudioRecordingConfig state ]
        ,  PrestoAnim.animationSet [ fadeIn $ not state.props.isAudioRecordingActive  ] $
              linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity CENTER_VERTICAL
                , margin $ Margin 10 16 10 0
                , visibility $ boolToInvisibility $ not state.props.isAudioRecordingActive 
                ]
                ( map
                    ( \item ->
                        SafetyActionTileView.view item.image item.text item.push item.backgroundColor item.strokeColor (V $ (EHC.screenWidth unit - 56) / 3) true item.isDisabled Color.white900 item.height
                    )
                    otherActions
                )        
      ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , visibility $ boolToVisibility $ state.props.showTestDrill && state.props.isAudioRecordingActive
        ]
        [ PrimaryButton.view (push <<< DismissTestDrill) $ dismissSoSButtonConfig state
        ]
    ]
    where   
      otherActions =
        [ { text: getString RECORD_AUDIO, image: fetchImage COMMON_ASSET "ny_ic_microphone_white", backgroundColor: Color.blackOpacity12, strokeColor: Color.black800, push : push <<< RecordAudio, isDisabled: false, height : MATCH_PARENT }
        , { text: getString CALL_POLICE , image: fetchImage COMMON_ASSET "ny_ic_police_alert" , backgroundColor: Color.redOpacity20, strokeColor: Color.redOpacity30, push : push <<< ShowPoliceView, isDisabled: state.props.showTestDrill, height : MATCH_PARENT  }
        , { text: getString CALL_SAFETY_TEAM, image: fetchImage FF_ASSET "ny_ic_support_unfilled", backgroundColor: Color.blackOpacity12, strokeColor: Color.black800, push : push <<< CallSafetyTeam, isDisabled: state.props.showTestDrill, height : WRAP_CONTENT }
        ]

orSeparatorView :: forall w. NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
orSeparatorView state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER_VERTICAL
    , margin $ MarginHorizontal 16 16
    ]
    [ linearLayout [ weight 1.0 ] [separatorView Color.black700 $ MarginRight 10]
    , textView
        $ [ text $ DS.toLower $ getString OR
          , color Color.white900
          ]
        <> FontStyle.tags TypoGraphy
    , linearLayout [ weight 1.0 ] [separatorView Color.black700 $ MarginLeft 10]
    ]

disclaimerView :: forall w. (Action -> Effect Unit) -> Visibility -> PrestoDOM (Effect Unit) w
disclaimerView push vis =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , stroke $ "1," <> Color.black700
    , orientation VERTICAL
    , margin $ Margin 16 16 16 0
    , padding $ Padding 16 16 16 16
    , cornerRadius 12.0
    , visibility vis
    ]
    [ textView
        $ [ text $ getString DISCLAIMER <> ":"
          , color Color.white900
          ]
        <> FontStyle.subHeading2 TypoGraphy
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ MarginTop 16
        , orientation VERTICAL
        ]
        ( map
            ( \item ->
                measureView
                  { text': item.text
                  , showBullet: true
                  , isCorrect: false
                  , color': Color.white900
                  , marginBottom: 10
                  , style: FontStyle.Body1
                  , action: item.action
                  }
                  push
            )
            disclaimerText
        )
    ]
  where
  disclaimerText =
    [ { text: getString USE_ONLY_IN_EMERGENCY, action: NoAction }
    , { text: getString USE_TEST_DRILL, action: GoToTestDrill }
    , { text: getString MISUSE_MAY_LEAD_TO_LEGAL_ACTION, action: NoAction }
    ]


type MeasureViewConfig
  = { text' :: String
    , showBullet :: Boolean
    , isCorrect :: Boolean
    , color' :: String
    , marginBottom :: Int
    , style :: FontStyle.Style
    , action :: Action
    }

measureView :: forall w. MeasureViewConfig -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
measureView { text', showBullet, isCorrect, color', marginBottom, style, action } push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ MarginBottom marginBottom
    , gravity LEFT
    ]
    [ textView
        $ [ text "•"
          , visibility $ boolToVisibility showBullet
          , gravity TOP_VERTICAL
          , height MATCH_PARENT
          , margin $ MarginRight 6
          , color color'
          ]
        <> (FontStyle.getFontStyle style LanguageStyle)
    , if not showBullet then
        imageView
          [ imageWithFallback $ fetchImage FF_ASSET if isCorrect then "ny_ic_tick_green" else "ny_ic_cross"
          , height $ V 20
          , width $ V 20
          , margin $ MarginRight 16
          , visibility $ boolToVisibility $ not showBullet
          ]
      else
        emptyTextView
    , textView
        $ [ textFromHtml text'
          , color color'
          , weight 1.0
          , gravity LEFT
          , onClick push $ const action
          ]
        <> (FontStyle.getFontStyle style LanguageStyle)
    ]

triggeringSosView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
triggeringSosView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    [ linearLayout 
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , margin $ Margin 16 16 16 16
      ][ SafetyActionTileView.view sirenImage (getString SIREN) (push <<< ToggleSiren) sirenTileBackgroundColor sirenTileStrokeColor MATCH_PARENT false false Color.white900 WRAP_CONTENT ]
    , warningView (getString SOS_WILL_BE_DISABLED) (not state.props.showTestDrill) true    
    ]
    where
      sirenImage = fetchImage COMMON_ASSET $ if state.props.triggerSiren then "ny_ic_full_volume" else "ny_ic_no_volume"
      sirenTileBackgroundColor = if state.props.triggerSiren then Color.black712 else Color.blackOpacity12
      sirenTileStrokeColor = if state.props.triggerSiren then Color.black700 else Color.black800

postRideSosView :: forall w . (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
postRideSosView push state = 
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , background Color.black900
  , orientation VERTICAL
  , padding $ PaddingBottom 20
  ][ recentRideView push state
  , actionListView push state
  , linearLayout[width MATCH_PARENT, weight 1.0][]
  , disclaimerView push VISIBLE
  ]

postRideHeaderConfig :: NammaSafetyScreenState -> Header.Config
postRideHeaderConfig state =
  (Header.config Language)
    { learnMoreTitle = getString LEARN_ABOUT_NAMMA_SAFETY
    , showLearnMore = true
    , useLightColor = true
    , title = getString SAFETY_CENTER
    , showCrossImage = not state.props.showCallPolice
    }


recentRideView :: forall w . (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
recentRideView push state = maybe (linearLayout[][]) (\ride -> 
  linearLayout
    [ margin (Margin 16 16 16 16)
    , background Color.blackOpacity12
    , cornerRadius 8.0
    , width MATCH_PARENT
    , orientation VERTICAL
    , stroke $ "1," <> Color.black700
    , height WRAP_CONTENT
    ][
      linearLayout
      [ height $ if EHC.os == "IOS" then V 134 else WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      ][  imageView
          [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_map_with_corners"
          , cornerRadii $ Corners 8.0 true false false false
          , height MATCH_PARENT
          , width $ V 130
          ]
        , layoutWithWeight
        , linearLayout
          [ height WRAP_CONTENT
          , width $ V $ (EHC.screenWidth unit) - 162
          , orientation VERTICAL
          , margin (MarginLeft 12)
          ][  dateAndTimeView ride
            , SourceToDestination.view (push <<< SourceToDestinationAC) (sourceToDestinationConfig ride)
            , driverRatingView ride
            ]
          ]
      ]) state.data.lastRideDetails


dateAndTimeView :: forall w .IndividualRideCardState -> PrestoDOM (Effect Unit) w
dateAndTimeView ride =
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , orientation HORIZONTAL
  , margin $ Margin 0 12 0 0
  , gravity CENTER_VERTICAL
  ][  textView $
      [ text ride.date
      , color Color.white900
      ] <> FontStyle.body16 LanguageStyle
    , linearLayout
      [ height MATCH_PARENT
      , width WRAP_CONTENT
      , gravity CENTER
      , orientation VERTICAL
      ][  linearLayout
          [ background Color.white900
          , cornerRadius 2.5
          , margin $ Margin 5 3 5 0
          , height $ V 5
          , width $ V 5
          ][]
       ]
    , textView $
      [ text ride.time
      , color Color.white900
      ] <> FontStyle.body16 LanguageStyle
    ]

driverRatingView :: forall w . IndividualRideCardState -> PrestoDOM (Effect Unit) w
driverRatingView ride =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER_VERTICAL
  , orientation HORIZONTAL
  , margin (Margin 0 13 0 10)
  ][  textView $
      [ text (getString YOU_RATED)
      , color Color.white900
      , accessibilityHint $ "You Rated : " <> (show ride.rating) <> " stars"
      , accessibility ENABLE
      ] <> FontStyle.captions LanguageStyle
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , padding (Padding 0 0 0 0)
      , margin (MarginLeft 4)
      , gravity LEFT
      ](map (\ item ->
                        linearLayout
                        [ height WRAP_CONTENT
                        , width WRAP_CONTENT
                        , margin if (item /= 5) then MarginRight 3 else MarginTop 0
                        ][imageView
                            [ height $ V 14
                            , width $ V 14
                            , imageWithFallback $ fetchImage FF_COMMON_ASSET $ if item <= ride.rating then "ny_ic_star_active" else "ny_ic_star_inactive"
                            ]
                          ]) [1 ,2 ,3 ,4 ,5])
    ]


actionListView :: forall w . (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
actionListView push state =
  let list = getPostRieSafetyAction state
      len = length list
  in
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , padding $  PaddingHorizontal 16 16
  ] (mapWithIndex (\idx item -> actionsListViewItem push item.action item.textConfig (idx /= (len -1))) list)

actionsListViewItem :: forall w. (Action -> Effect Unit) -> Maybe Action -> SU.MeasureViewConfig -> Boolean -> PrestoDOM (Effect Unit) w
actionsListViewItem push mbAction textConfig showDivider =
  linearLayout[
    height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  ]$[linearLayout
    ([ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER_VERTICAL
    , margin $ MarginTop 20
    ] <> maybe [] (\action -> [ onClick push $ const action]) mbAction)
      [ SU.measureView textConfig
      , layoutWithWeight
      , imageView
          [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_right_white"
          , height $ V 20
          , width $ V 20
          ]
      ]] <> if showDivider then [separatorView Color.black700 $ MarginTop 16 ] else []
  
getPostRieSafetyAction :: NammaSafetyScreenState -> Array {action :: Maybe Action, textConfig :: SU.MeasureViewConfig}
getPostRieSafetyAction state = [
  {action : Just AlertSafetyTeam
  , textConfig :  defaultTextConfig { text' = getString $  ALERT_SAFETY_TEAM state.props.appName
    , image = Just "ny_ic_notify_safety_bell"
    } 
  },
   {action : Just $ ShowPoliceView SafetyActionTileView.OnClick
  , textConfig : defaultTextConfig { text' = getString CALL_POLICE
    , image = Just "ny_ic_police"
    }
  },
   {action : Just ShowSafetyIssueView
  , textConfig : defaultTextConfig { text' = getString REPORT_SAFETY_ISSUE
    , image = Just "ny_ic_issue_box"
    }
  }
]

defaultTextConfig :: SU.MeasureViewConfig
defaultTextConfig =  { 
    text': ""
    , isActive: true
    , textColor: Color.white900
    , useMargin: false
    , usePadding: false
    , useFullWidth: false
    , image: Nothing
    , visibility: true
    , textStyle: FontStyle.Tags
    , showBullet: false
    }

confirmSafetyDrillView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
confirmSafetyDrillView state push =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding $ PaddingHorizontal 16 16
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , padding $ Padding 0 16 16 16
        , onClick push $ const BackPressed
        , accessibilityHint "Back : button"
        ]
        [ imageView
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_left_white"
            , height $ V 24
            , width $ V 24
            ]
        ]
    , imageView
        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_start_test_drill"
        , width MATCH_PARENT
        , height $ V 200
        ]
    , textView $
      [ text $ getString PREPARE_EMERGENCY_CONTACTS
      , color Color.white900
      , margin $ MarginTop 16
      ] <> FontStyle.h2 TypoGraphy
      
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ]
        [ SU.measureView configActionOne
        , SU.measureView configActionTwo
        ]
    , layoutWithWeight
    , PrimaryButton.view (push <<< StartTestDrill) $ startTestDrillButtonConfig state
    ]
  where
    configActionOne =
      { text': getString EMERGENCY_CONTACTS_WILL_BE_NOTIFIED
      , isActive: true
      , textColor: Color.white900
      , useMargin: true
      , usePadding: false
      , useFullWidth: true
      , image: Nothing
      , visibility: true
      , textStyle: FontStyle.Body1
      , showBullet: false
      }

    configActionTwo =
      { text': getString INFORM_EMERGENCY_CONTACTS_ABOUT_TEST
      , isActive: true
      , textColor: Color.white900
      , useMargin: true
      , usePadding: false
      , useFullWidth: true
      , image: Nothing
      , visibility: true
      , textStyle: FontStyle.Body1
      , showBullet: false
      }

warningView :: forall w. String -> Boolean -> Boolean -> PrestoDOM (Effect Unit) w
warningView text' visibility' useMargin =
  linearLayout
    ( [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER
      , padding $ PaddingVertical 12 12
      , cornerRadius 8.0
      , background Color.redOpacity20
      , visibility $ boolToVisibility visibility'
      ]
        <> if useMargin then [ margin $ Margin 16 16 16 0 ] else [ margin $ MarginTop 16 ]
    )
    [ imageView
        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_alert_triangle_white"
        , height $ V 16
        , width $ V 16
        ]
    , textView
        $ [ text text'
          , margin $ MarginLeft 8
          , color Color.white900
          ]
        <> FontStyle.body3 TypoGraphy
    ]

dialPoliceView :: forall w. NammaSafetyScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
dialPoliceView state push =
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding $ PaddingHorizontal 16 16
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , stroke $ "1," <> Color.black700
            , background Color.blackOpacity12
            , orientation VERTICAL
            , margin $ MarginVertical 16 16
            , padding $ Padding 16 16 16 16
            , cornerRadius 12.0
            ]
            [ linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity CENTER_VERTICAL
                ]
                [ imageView
                    [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_map_pin_white"
                    , height $ V 24
                    , width $ V 24
                    , margin $ MarginRight 10
                    ]
                , linearLayout
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    , orientation VERTICAL
                    ]
                    [ textView
                        $ [ text $ getString YOUR_CURRENT_LOCATION
                          , color Color.white900
                          ]
                        <> FontStyle.subHeading1 TypoGraphy
                    , textView
                        $ [ text state.data.currentLocation
                          , color Color.white900
                          , margin $ MarginTop 4
                          ]
                        <> FontStyle.paragraphText TypoGraphy
                    ]
                ]
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity CENTER_VERTICAL
                , margin $ MarginTop 16
                ]
                [ imageView
                    [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_vehicle_details"
                    , height $ V 24
                    , width $ V 24
                    , margin $ MarginRight 10
                    ]
                , linearLayout
                    [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    , orientation VERTICAL
                    ]
                    [ textView
                        $ [ text $ getString YOUR_VEHICLE_INFO
                          , color Color.white900
                          ]
                        <> FontStyle.subHeading1 TypoGraphy
                    , textView
                        $ [ text $ getVehicleDetails state
                          , color Color.white900
                          , margin $ MarginTop 4
                          ]
                        <> FontStyle.paragraphText TypoGraphy
                    ]
                ]
            , separatorView Color.black500 $ Margin 16 16 16 16
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity CENTER_VERTICAL
                ]
                [ imageView
                    [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_info"
                    , height $ V 24
                    , width $ V 24
                    , margin $ MarginRight 5
                    ]
                , textView
                    $ [ text $ getString POLICE_VIEW_INSTRUCTION
                      , color Color.black500
                      ]
                    <> FontStyle.tags TypoGraphy
                ]
            ]
        , warningView (getString SAFETY_CENTER_IS_DISABLED) state.props.isSafetyCenterDisabled false
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , alignParentBottom "true,-1"
        , orientation VERTICAL
        , margin $ MarginBottom 16
        ]
        [ safetyPartnerView Language
        , callPoliceView state push
        ]
    ]

callPoliceView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
callPoliceView state push =
  linearLayout
    ( [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER
      , padding $ PaddingVertical 12 12
      , cornerRadius 8.0
      , background Color.redOpacity20
      , accessibilityHint "Call Police Button"
      ]
        <> if state.props.showTestDrill then [ alpha 0.6 ] else [ rippleColor Color.rippleShade, onClick push $ const $ CallPolice ]
    )
    [ imageView
        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_police"
        , height $ V 26
        , width $ V 26
        ]
    , textView
        $ [ text $ getString $ DIALING_POLICE_IN_TIME $ show state.props.policeCallTimerValue
          , gravity CENTER
          , color Color.white900
          , margin $ MarginLeft 6
          ]
        <> FontStyle.subHeading2 TypoGraphy
    ]

menuOptionModal :: forall w. (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
menuOptionModal push state = 
  linearLayout 
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , padding $ PaddingTop 55
    , background Color.blackLessTrans
    ][ OptionsMenu.view (push <<< OptionsMenuAction) (optionsMenuConfig state) ]

contactsPopupView :: forall w. (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
contactsPopupView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , padding $ PaddingTop 55
    , background Color.blackLessTrans
    ]
    [ PopupWithCheckbox.view (push <<< PopupWithCheckboxAction) (defaultCallContactPopupConfig state) ]