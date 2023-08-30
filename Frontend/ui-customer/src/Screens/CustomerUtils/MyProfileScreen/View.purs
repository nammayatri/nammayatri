{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.MyProfileScreen.View where

import Common.Types.App
import Screens.CustomerUtils.MyProfileScreen.ComponentConfig

import Animation as Anim
import Components.GenericHeader as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Control.Monad.Except (lift, runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array (mapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getCommonAssetStoreLink, getAssetStoreLink)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, discard, not, pure, unit, (-), ($), (<<<), (==), (||), (/=), (<>))
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), Accessiblity(..), background, color, cornerRadius, fontStyle, frameLayout, gravity, height, imageUrl, imageView, linearLayout, margin, onBackPressed, orientation, padding, text, textSize, textView, width, afterRender, onClick, visibility, alignParentBottom, weight, imageWithFallback, editText, onChange, hint, hintColor, pattern, id, singleLine, stroke, clickable, inputTypeI, hintColor, relativeLayout, scrollView, frameLayout, scrollBarY, onAnimationEnd, adjustViewWithKeyboard, accessibilityHint, accessibilityImportance)
import Resources.Constants as RSRC
import PrestoDOM.Animation as PrestoAnim
import Screens.MyProfileScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Services.Backend as Remote
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color
import Types.App (defaultGlobalState)
import Engineering.Helpers.Utils (toggleLoader, loaderText)
import Data.String as DS


screen :: ST.MyProfileScreenState -> Screen Action ST.MyProfileScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "MyProfileScreen"
  , globalEvents : [(\push -> do
                      _ <- launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT $ do
                        lift $ lift $ loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
                        lift $ lift $ toggleLoader true
                        response <- Remote.getProfileBT ""
                        lift $ lift $ toggleLoader false
                        if initialState.props.isEmailValid then
                          lift $ lift $ doAff do liftEffect $ push $ UserProfile response
                          else pure unit
                        pure unit
                      pure $ pure unit
                    )
                  ]
  , eval
  }

view :: forall w . (Action -> Effect Unit) -> ST.MyProfileScreenState -> PrestoDOM (Effect Unit) w
view push state =
    Anim.screenAnimation $
      frameLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , background Color.white900
      , adjustViewWithKeyboard "true"
      ][
        linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , gravity BOTTOM
            , alignParentBottom "true,-1"
            , padding $ PaddingBottom 24
            , visibility if state.props.updateProfile then VISIBLE else GONE
            , background Color.white900
            ][ linearLayout
                [ orientation VERTICAL
                , height WRAP_CONTENT
                , width MATCH_PARENT
                ]
                [ PrimaryButton.view (push <<< UpdateButtonAction) (updateButtonConfig state)
                ]
              ]
        , linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , onBackPressed push (const BackPressed state)
            , afterRender push (const AfterRender)
            , padding (PaddingBottom EHC.safeMarginBottom)
            , margin $ MarginBottom if EHC.os == "IOS" then (if state.props.updateProfile then 85 else 65) else 75
            , background Color.white900
            ][  headerView state push
              , linearLayout
                [ height $ V 1
                , width MATCH_PARENT
                , background Color.greySmoke
                , visibility if state.data.config.nyBrandingVisibility then GONE else VISIBLE
                ][]
              , detailsView state push
            ]
          ]

detailsView :: forall w. ST.MyProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
detailsView state push =
  relativeLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  ]
  [ personalDetails state push
  , updatePersonalDetails state push
  ]

personalDetails :: forall w. ST.MyProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
personalDetails state push =
  linearLayout
  [ height  if EHC.os == "IOS" then V (EHC.screenHeight unit) else MATCH_PARENT
  , width MATCH_PARENT
  , padding $ PaddingBottom if EHC.os == "IOS" then 50 else 15
  , background Color.white900
  , orientation VERTICAL
  , visibility if state.props.updateProfile then GONE else VISIBLE
  ][  scrollView[
          height WRAP_CONTENT
        , width MATCH_PARENT
        , background Color.white900
        , scrollBarY false
        ][
           linearLayout
              [ height if EHC.os == "IOS" then (V (EHC.screenHeight unit)) else WRAP_CONTENT
              , width MATCH_PARENT
              , orientation VERTICAL
              , background Color.white900
              , padding $ PaddingBottom 100
              ]
              [ profileImageView state push
              , linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , background Color.white900
              , orientation VERTICAL
              ]
              ( mapWithIndex ( \index item ->
                    linearLayout
                      [ height WRAP_CONTENT
                      , width MATCH_PARENT
                      , orientation VERTICAL
                      ]
                      [ linearLayout
                          [ height WRAP_CONTENT
                          , width MATCH_PARENT
                          , orientation VERTICAL
                          , margin $ Margin 16 0 16 0
                          ][  textView $
                              [ height WRAP_CONTENT
                              , width MATCH_PARENT
                              , text item.title
                              , clickable false
                              , color Color.black700
                              , margin $ MarginBottom 8
                              ] <> FontStyle.body3 LanguageStyle
                            , textView $
                              [ height WRAP_CONTENT
                              , width MATCH_PARENT
                              , text item.text
                              , accessibilityHint $ case item.fieldType of
                                 ST.MOBILE ->  (DS.replaceAll (DS.Pattern "") (DS.Replacement "-") item.text) 
                                 _ -> item.text
                              , accessibilityImportance ENABLE
                              , color case item.fieldType of
                                  ST.EMAILID_ ->  if state.data.emailId /= Nothing then Color.black900 else Color.blue900
                                  ST.GENDER_ -> if state.data.gender /= Nothing then Color.black900 else Color.blue900
                                  _ -> Color.black900
                              , onClick push $ case item.fieldType of
                                  ST.EMAILID_ ->  if state.data.emailId /= Nothing then const $ NoAction else const $ EditProfile $ Just ST.EMAILID_
                                  ST.GENDER_ -> if state.data.gender /= Nothing then const $ NoAction  else const $ EditProfile $ Just ST.GENDER_
                                  _ -> const $ NoAction
                              , clickable case item.fieldType of
                                  ST.EMAILID_ ->  not (isJust state.data.emailId)  
                                  ST.GENDER_ -> not ( isJust state.data.gender) 
                                  _ -> false
                              ] <> FontStyle.body6 LanguageStyle
                            ]
                      , horizontalLineView state (index /= 3)
                      ] ) (personalDetailsArray state))
              ]
          ]
      ]

personalDetailsArray :: ST.MyProfileScreenState ->  Array {title :: String, text :: String, fieldType :: ST.FieldType}
personalDetailsArray state =
  [ {title : (getString NAME), text : state.data.name, fieldType : ST.NAME}
  , {title : (getString EMAIL_ID) , text :fromMaybe (getString ADD_NOW) state.data.emailId , fieldType : ST.EMAILID_ }
  , {title : (getString GENDER_STR), text : (RSRC.getGender state.data.gender (getString SET_NOW)), fieldType : ST.GENDER_}
  , {title : (getString MOBILE) , text : (getValueToLocalStore MOBILE_NUMBER), fieldType : ST.MOBILE}
  ]

horizontalLineView :: forall w. ST.MyProfileScreenState -> Boolean -> PrestoDOM (Effect Unit) w
horizontalLineView state visible =
  linearLayout
    [ height $ V 1
    , width MATCH_PARENT
    , margin $ Margin 16 20 16 16
    , background Color.greySmoke
    , visibility if visible then VISIBLE else GONE
    ][]

updatePersonalDetails :: forall w. ST.MyProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
updatePersonalDetails state push =
  linearLayout
  [ height  if EHC.os == "IOS" then V (EHC.screenHeight unit) else MATCH_PARENT
  , width MATCH_PARENT
  , padding $ PaddingBottom if EHC.os == "IOS" then 75 else 0
  , background Color.white900
  , orientation VERTICAL
  , visibility if state.props.updateProfile then VISIBLE else GONE
  ][  scrollView[
          height WRAP_CONTENT
        , width MATCH_PARENT
        , background Color.white900
        , scrollBarY false
        ][
           linearLayout
              [ height if EHC.os == "IOS" then (V (EHC.screenHeight unit)) else WRAP_CONTENT
              , width MATCH_PARENT
              , orientation VERTICAL
              , background Color.white900
              ]
              ([] <> (if state.props.updateProfile then [userNameEditTextView push state, emailIdEditTextView push state, genderCaptureView state push ] else []))
          ]
      ]

headerView :: forall w. ST.MyProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
headerView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
    , padding (PaddingTop EHC.safeMarginTop)
    ][  GenericHeader.view (push <<< GenericHeaderActionController) (genericHeaderConfig state)
      , linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , gravity RIGHT
        , margin (Margin 0 0 25 0)
        , visibility if not state.props.updateProfile then VISIBLE else GONE
        ][ linearLayout
          [ width WRAP_CONTENT
          , height MATCH_PARENT
          , gravity $ case state.data.config.profileEditGravity of 
              "bottom" -> BOTTOM
              _ -> CENTER
          , orientation VERTICAL
          ][ textView $
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , text (getString EDIT)
              , accessibilityHint $ "Edit Profile : Button"
              , accessibilityImportance ENABLE
              , color Color.blueTextColor
              , padding $ PaddingBottom (if state.data.config.profileEditGravity == "bottom" then 10 else 0)
              , onClick push (const $ EditProfile Nothing)
              ] <> FontStyle.subHeading1 LanguageStyle
            ]
          ]
      ]

profileImageView :: forall w. ST.MyProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
profileImageView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER
    , margin (Margin 16 50 16 60)
    ][  frameLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        ][ linearLayout
            [ height $ V 100
            , width $ V 100
            , cornerRadius 50.0
            , gravity CENTER
            ][  imageView
                [ height $ V 100
                , width $ V 100
                , imageWithFallback $ "ny_ic_profile_image," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_profile_image.png"
                ]
            ]
          ]
      ]

userNameEditTextView :: forall w . (Action -> Effect Unit) -> ST.MyProfileScreenState -> PrestoDOM (Effect Unit) w
userNameEditTextView push state =
  PrimaryEditText.view (push <<< NameEditTextAction) (nameEditTextConfig state)

mobileNumberTextView :: forall w . ST.MyProfileScreenState -> PrestoDOM (Effect Unit) w
mobileNumberTextView state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ MarginTop 32
  ][  textView $
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , text (getString MOBILE_NUMBER_STR)
      , color Color.black700
      , margin $ MarginBottom 8
      ] <> FontStyle.body3 LanguageStyle
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , padding $ Padding 20 17 20 17
      , cornerRadius 8.0
      , stroke $ "1,"<>Color.greySmoke
      ][textView $
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , text $ getValueToLocalStore MOBILE_NUMBER
      , color Color.black600
      ] <> FontStyle.subHeading1 LanguageStyle ]
  ]


emailIdEditTextView :: forall w . (Action -> Effect Unit) -> ST.MyProfileScreenState -> PrestoDOM (Effect Unit) w
emailIdEditTextView push state =
  PrimaryEditText.view (push <<< EmailIDEditTextAction) (emailEditTextConfig state)


genderCaptureView :: forall w. ST.MyProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
genderCaptureView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ Margin 16 32 16 0
    , background Color.white900
    , orientation VERTICAL
    ] $
    [ textView $
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , text $ getString GENDER_STR
      , color Color.black800
      , gravity LEFT
      , singleLine true
      , margin $ MarginBottom 12
      ] <> FontStyle.body3 LanguageStyle
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding $ Padding 20 15 20 15
        , cornerRadius 8.0
        , onClick push (const ShowOptions)
        , stroke $ "1,"<> Color.borderColorLight
        , gravity CENTER_VERTICAL
        ]
        [ textView $
          [ text $ RSRC.getGender state.data.editedGender (getString SELECT_YOUR_GENDER)
          , height WRAP_CONTENT
          , width WRAP_CONTENT
          , accessibilityImportance ENABLE
          , accessibilityHint $ if state.data.editedGender == Nothing then "Select your gender : Drop-Down menu" else "Gender Selected : " <> RSRC.getGender state.data.editedGender (getString SELECT_YOUR_GENDER) <> " : " <>  if state.props.genderOptionExpanded then "Double Tap To Collapse DropDown" else " Double Tap To Expand DropDown"--if state.props.genderOptionExpanded then "Double Tap To Close DropDown" else "Gender Drop Down Menu : Double Tap To Change Gender"
          , color if state.data.editedGender == Nothing then Color.black600 else Color.black800
          ] <> FontStyle.subHeading1 LanguageStyle
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity RIGHT
            ]
            [ imageView
              [ imageWithFallback if state.props.genderOptionExpanded then "ny_ic_chevron_up," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_chevron_up.png" else "ny_ic_chevron_down," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_chevron_down.png"
              , height $ V 24
              , width $ V 15
              ]
            ]
        ]
      , relativeLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        ]
        [  mobileNumberTextView state
        , if state.props.expandEnabled then genderOptionsView state push else textView[height $ V 0]
        ]
    ]




genderOptionsView :: forall w. ST.MyProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
genderOptionsView state push =
  PrestoAnim.animationSet
  ([] <> if EHC.os == "IOS" then
        [Anim.fadeIn state.props.genderOptionExpanded
        , Anim.fadeOut  (not state.props.genderOptionExpanded)]
        else
          [Anim.listExpandingAnimation $  listExpandingAnimationConfig state] )
            $

  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ MarginVertical 8 8
    , background Color.grey700
    , orientation VERTICAL
    , stroke $ "1,"<>Color.grey900
    , visibility $ if (state.props.genderOptionExpanded || state.props.showOptions) then VISIBLE else GONE
    , onAnimationEnd push $ AnimationEnd
    , cornerRadius 8.0
    ]
    (mapWithIndex(\index item ->
       linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , onClick push $ const $ GenderSelected item.value
        , accessibilityHint $ item.text <> " : Double Tap To Select"
        , accessibilityImportance ENABLE
        , orientation VERTICAL
        ]
        [ textView $
          [ text item.text
          , color Color.black900
          , margin $ Margin 16 15 16 15
          ] <> FontStyle.paragraphText LanguageStyle
        , linearLayout
          [ height $ V 1
          , width MATCH_PARENT
          , background Color.grey900
          , visibility if index == 3 then GONE else VISIBLE
          , margin $ MarginHorizontal 16 16
          ][]
        ]
       )(genderOptionsArray state)

    )

genderOptionsArray :: ST.MyProfileScreenState -> Array {text :: String, value :: ST.Gender}
genderOptionsArray state =
  [ {text : (getString FEMALE), value : ST.FEMALE}
  , {text : (getString MALE), value : ST.MALE}
  , {text : (getString OTHER) , value : ST.OTHER}
  , {text : (getString PREFER_NOT_TO_SAY) , value : ST.PREFER_NOT_TO_SAY}
  ]

