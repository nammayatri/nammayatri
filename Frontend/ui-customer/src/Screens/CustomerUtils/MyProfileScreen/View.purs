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
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons as EHC 
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, discard, not, pure, unit, ($), (<<<), (==))
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), background, color, cornerRadius, fontStyle, frameLayout, gravity, height, imageUrl, imageView, linearLayout, margin, onBackPressed, orientation, padding, text, textSize, textView, width, afterRender, onClick, visibility, alignParentBottom, weight, imageWithFallback)
import Screens.MyProfileScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST 
import Services.Backend as Remote
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color
import Common.Types.App
import Screens.CustomerUtils.MyProfileScreen.ComponentConfig
import Constant.Test as Id
import EN

screen :: ST.MyProfileScreenState -> Screen Action ST.MyProfileScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "MyProfileScreen"
  , globalEvents : [(\push -> do
                      launchAff_ $ EHC.flowRunner $ runExceptT $ runBackT $ do
                        response <- Remote.getProfileBT ""
                        lift $ lift $ doAff do liftEffect $ push $ UserProfile response
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
    ][ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , onBackPressed push (const BackPressed state)
        , afterRender push (const AfterRender)
        , padding (PaddingBottom EHC.safeMarginBottom)
        , Id.testId $ Id.Screen Id.myProfileScreen
    ][ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        , padding (PaddingTop EHC.safeMarginTop)
        ][  GenericHeader.view (push <<< GenericHeaderActionController) (genericHeaderConfig state)
         ,  linearLayout
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
                  , onClick push (const EditProfile)
                  , Id.testId $ Id.Element (getEN EDIT)
                  ]
                ]
             ]
        ]
      , linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , background Color.greySmoke
        ][]
      , profileImageView state push
      , textView
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , textSize FontSize.a_12
        , text (getString NAME)
        , color Color.black700
        , fontStyle $ FontStyle.regular LanguageStyle
        , margin (Margin 16 10 16 0)
        , visibility if state.props.updateProfile then GONE else VISIBLE
        ]  
      , if state.props.updateProfile then (userNameEditTextView push state) else (userNameTextView state)
      , linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , background Color.greySmoke
        , margin (Margin 16 10 16 0)
        , visibility if state.props.updateProfile then GONE else VISIBLE
        ][] 
      , textView
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , textSize FontSize.a_12
        , text (getString MOBILE_NUMBER_STR)
        , color Color.black700
        , fontStyle $ FontStyle.regular LanguageStyle
        , margin (Margin 16 10 16 0)
        ]  
      , userMobileNumberView state
      , linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , gravity BOTTOM
        , alignParentBottom "true,-1"
        , padding (PaddingBottom (if EHC.safeMarginBottom == 0 then 24 else 0))
        , visibility if state.props.updateProfile then VISIBLE else GONE
        ][PrimaryButton.view (push <<< UpdateButtonAction) (updateButtonConfig state)]
      , deleteAccountView state push]
      , linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , background Color.lightBlack900
        , visibility if (state.props.accountStatus == ST.CONFIRM_REQ) then VISIBLE else GONE
        ][ PopUpModal.view (push <<<  PopUpModalAction) (requestDeletePopUp state )
        ]
      , linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , background Color.lightBlack900
        , visibility if (state.props.accountStatus == ST.DEL_REQUESTED )then VISIBLE else GONE 
        ][PopUpModal.view (push <<< AccountDeletedModalAction) (accountDeletedPopUp state)]
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
            , margin (Margin 16 0 16 0)
            , background Color.greySmoke
            ][]
          , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            , gravity CENTER_VERTICAL
            , padding (Padding 15 21 15 21)
            , onClick push (const $ ReqDelAccount)
            , Id.testId $ Id.Container (getEN REQUEST_TO_DELETE_ACCOUNT)
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

profileImageView :: forall w . ST.MyProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
profileImageView state push = 
  linearLayout 
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER
    , margin (Margin 16 50 16 60)
    , visibility if state.props.updateProfile then GONE else VISIBLE
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

userNameTextView :: forall w . ST.MyProfileScreenState -> PrestoDOM (Effect Unit) w
userNameTextView state = 
  textView
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , textSize FontSize.a_14
    , text state.data.name
    , color Color.black900
    , fontStyle $ FontStyle.medium LanguageStyle
    , margin (Margin 16 10 16 0)
    ] 

userNameEditTextView :: forall w . (Action -> Effect Unit) -> ST.MyProfileScreenState -> PrestoDOM (Effect Unit) w
userNameEditTextView push state = 
  PrimaryEditText.view (push <<< NameEditTextAction) (nameEditTextConfig state) 

userMobileNumberView :: forall w . ST.MyProfileScreenState -> PrestoDOM (Effect Unit) w
userMobileNumberView state = 
  textView
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , textSize FontSize.a_14
    , text (getValueToLocalStore MOBILE_NUMBER)
    , color Color.black900
    , fontStyle $ FontStyle.medium LanguageStyle
    , margin (Margin 16 10 16 0)
    ]
