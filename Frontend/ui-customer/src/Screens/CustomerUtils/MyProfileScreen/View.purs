{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.MyProfileScreen.View where

import Animation as Anim
import Components.GenericHeader as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, discard, not, pure, unit, (-), ($), (<<<), (==), (||), (/=), (<>))
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), background, color, cornerRadius, fontStyle, frameLayout, gravity, height, imageUrl, imageView, linearLayout, margin, onBackPressed, orientation, padding, text, textSize, textView, width, afterRender, onClick, visibility, alignParentBottom, weight, imageWithFallback, editText, onChange, hint, hintColor, pattern, id, singleLine, stroke, clickable, inputTypeI, hintColor, relativeLayout, scrollView, frameLayout, scrollBarY, onAnimationEnd)
import Screens.MyProfileScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Services.Backend as Remote
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color
import Common.Types.App
import Data.Array (mapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Screens.CustomerUtils.MyProfileScreen.ComponentConfig
import PrestoDOM.Animation as PrestoAnim
import Resources.Constants as RSRC

screen :: ST.MyProfileScreenState -> Screen Action ST.MyProfileScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "MyProfileScreen"
  , globalEvents : [(\push -> do
                      _ <- launchAff $ EHC.flowRunner $ runExceptT $ runBackT $ do
                        response <- Remote.getProfileBT ""
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
                [ linearLayout
                  [ background Color.borderColorLight
                  , height $ V 1
                  , margin $ MarginBottom if EHC.os == "IOS" then 12 else 2
                  , width MATCH_PARENT
                  ][]
                , PrimaryButton.view (push <<< UpdateButtonAction) (updateButtonConfig state)
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
                ][]
              , detailsView state push
            ]
          , deleteAccountView state push
          , linearLayout
              [ width MATCH_PARENT
              , height MATCH_PARENT
              , background Color.lightBlack900
              , visibility if (state.props.accountStatus == ST.CONFIRM_REQ) then VISIBLE else GONE
              ][ PopUpModal.view (push <<<  PopUpModalAction) (requestDeletePopUp state ) ]
          , linearLayout
              [ width MATCH_PARENT
              , height MATCH_PARENT
              , background Color.lightBlack900
              , visibility if (state.props.accountStatus == ST.DEL_REQUESTED )then VISIBLE else GONE
              ][ PopUpModal.view (push <<< AccountDeletedModalAction) (accountDeletedPopUp state) ]
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
                          ][  textView
                              [ height WRAP_CONTENT
                              , width MATCH_PARENT
                              , textSize FontSize.a_12
                              , text item.title
                              , color Color.black700
                              , margin $ MarginBottom 8
                              , fontStyle $ FontStyle.regular LanguageStyle
                              ]
                            , textView
                              [ height WRAP_CONTENT
                              , width MATCH_PARENT
                              , textSize FontSize.a_14
                              , text item.text
                              , color case item.fieldType of
                                  ST.EMAILID_ ->  if state.data.emailId /= Nothing then Color.black900 else Color.blue900
                                  ST.GENDER_ -> if state.data.gender /= Nothing then Color.black900 else Color.blue900
                                  _ -> Color.black900
                              , onClick push $ case item.fieldType of
                                  ST.EMAILID_ ->  if state.data.emailId /= Nothing then const $ NoAction else const $ EditProfile $ Just ST.EMAILID_
                                  ST.GENDER_ -> if state.data.gender /= Nothing then const $ NoAction  else const $ EditProfile $ Just ST.GENDER_
                                  _ -> const $ NoAction
                              , fontStyle $ FontStyle.semiBold LanguageStyle
                              ]
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
              [ if state.props.updateProfile then userNameEditTextView push state else textView[]
              , emailIdEditTextView push state
              , genderCaptureView state push
              ]
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
          , gravity CENTER
          , orientation VERTICAL
          ][ textView
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , textSize FontSize.a_16
              , text (getString EDIT)
              , color Color.blueTextColor
              , fontStyle $ FontStyle.semiBold LanguageStyle
              , onClick push (const $ EditProfile Nothing)
              ]
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
                , imageWithFallback "ny_ic_profile_image,https://assets.juspay.in/nammayatri/images/common/ny_ic_profile_image.png"
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
  ][  textView
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , textSize FontSize.a_12
      , text (getString MOBILE_NUMBER_STR)
      , color Color.black700
      , margin $ MarginBottom 8
      , fontStyle $ FontStyle.regular LanguageStyle
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , padding $ Padding 20 17 20 17
      , cornerRadius 8.0
      , stroke $ "1,"<>Color.greySmoke
      ][textView
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , textSize FontSize.a_16
      , text $ getValueToLocalStore MOBILE_NUMBER
      , color Color.black600
      , fontStyle $ FontStyle.semiBold LanguageStyle
      ] ]
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
    [ textView
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , text $ getString GENDER_STR
      , color Color.black800
      , gravity LEFT
      , fontStyle $ FontStyle.regular LanguageStyle
      , singleLine true
      , textSize FontSize.a_12
      , margin $ MarginBottom 12
      ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding $ Padding 20 15 20 15
        , cornerRadius 8.0
        , onClick push (const ShowOptions)
        , stroke $ "1,"<> Color.borderColorLight
        , gravity CENTER_VERTICAL
        ]
        [ textView
          [ text $ RSRC.getGender state.data.editedGender (getString SELECT_YOUR_GENDER)
          , textSize FontSize.a_16
          , fontStyle $ FontStyle.semiBold LanguageStyle
          , height WRAP_CONTENT
          , width WRAP_CONTENT
          , color if state.data.editedGender == Nothing then Color.black600 else Color.black800
          ]
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity RIGHT
            ]
            [ imageView
              [ imageWithFallback if state.props.genderOptionExpanded then "ny_ic_chevron_up,https://assets.juspay.in/nammayatri/images/common/ny_ic_chevron_up.png" else "ny_ic_chevron_down,https://assets.juspay.in/nammayatri/images/common/ny_ic_chevron_down.png"
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
        , orientation VERTICAL
        ]
        [ textView
          [ text item.text
          , textSize FontSize.a_14
          , fontStyle $ FontStyle.regular LanguageStyle
          , color Color.black900
          , margin $ Margin 16 15 16 15
          ]
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

deleteAccountView :: forall w . ST.MyProfileScreenState -> (Action -> Effect Unit) ->  PrestoDOM (Effect Unit) w
deleteAccountView state push =
  linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , gravity BOTTOM
        , visibility if state.props.updateProfile then GONE else VISIBLE
        ][  linearLayout
            [ height $ V 1
            , width MATCH_PARENT
            , background Color.greySmoke
            ][]
          , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            , gravity CENTER_VERTICAL
            , padding (Padding 15 21 15 21)
            , onClick push (const $ ReqDelAccount)
            ][  imageView
                [ width $ V 20
                , height $ V 20
                , imageWithFallback "ny_ic_trash,https://assets.juspay.in/nammayatri/images/user/ny_ic_trash.png"
                ]
                , textView
                [ height WRAP_CONTENT
                , weight 1.0
                , text (getString REQUEST_TO_DELETE_ACCOUNT)
                , margin (MarginLeft 10)
                , textSize FontSize.a_14
                , color Color.red
                , fontStyle $ FontStyle.medium LanguageStyle
                ]
                , linearLayout
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , orientation HORIZONTAL
                , gravity CENTER_VERTICAL
                ][ imageView
                    [ width $ V 15
                    , height $ V 15
                    , imageWithFallback "ny_ic_chevron_right,https://assets.juspay.in/nammayatri/images/user/ny_ic_chevron_right.png"
                    ]
                  ]
              ]
          ]
