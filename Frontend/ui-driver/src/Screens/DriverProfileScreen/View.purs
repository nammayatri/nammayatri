{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DriverProfileScreen.View where

import Prelude (Unit, ($), const, map, (==), (||), (/), unit, bind, (-), (<>), (<<<), pure, discard, show)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), background, color, fontStyle, gravity, height, imageUrl, imageView, layoutGravity, linearLayout, margin, orientation, padding, text, textSize, textView, weight, width, onClick, frameLayout, alpha, scrollView, cornerRadius, onBackPressed, visibility, id, afterRender, imageWithFallback)
import Effect (Effect)
import Screens.DriverProfileScreen.Controller (Action(..), ScreenOutput, eval, getTitle)
import Screens.DriverProfileScreen.ScreenData (MenuOptions(..), optionList)
import Components.BottomNavBar.Controller (navData)
import Screens.Types as ST
import Styles.Colors as Color
import Font.Style as FontStyle
import Font.Size as FontSize
import Language.Types (STR(..))
import Language.Strings (getString)
import Engineering.Helpers.Commons as EHC
import Animation as Anim
import Components.BottomNavBar.View as BottomNavBar
import Storage (KeyStore(..),getValueToLocalStore)
import JBridge as JB
import Effect.Class (liftEffect)
import Services.Backend as Remote
import Services.APITypes(GetDriverInfoReq(..), GetDriverInfoResp(..))
import Control.Monad.Trans.Class (lift)
import Presto.Core.Types.Language.Flow (doAff)
import Effect.Aff (launchAff_)
import Control.Monad.Except (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Data.Maybe (fromMaybe)
import Components.PopUpModal as PopUpModal
import Common.Types.App
import Screens.DriverProfileScreen.ComponentConfig


screen :: ST.DriverProfileScreenState -> Screen Action ST.DriverProfileScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "DriverProfileScreen"
  , globalEvents : [(\push -> do
      launchAff_ $ EHC.flowRunner $ runExceptT $ runBackT $ do
        getDriverInfoResp <- Remote.getDriverInfoBT (GetDriverInfoReq { })
        let (GetDriverInfoResp getDriverInfoResp) = getDriverInfoResp
        lift $ lift $ doAff do liftEffect $ push $ GetDriverInfoResponse (GetDriverInfoResp getDriverInfoResp)
        pure unit
      pure $ pure unit
    )]
  , eval
  }

view
  :: forall w
  . (Action -> Effect Unit)
  -> ST.DriverProfileScreenState
  -> PrestoDOM (Effect Unit) w
view push state =
  frameLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    ][ linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , background Color.white900
      , onBackPressed push (const BackPressed state)
      , afterRender push (const AfterRender)
      ][ Anim.screenAnimationFadeInOut $
          linearLayout
          [ width MATCH_PARENT
          , height MATCH_PARENT
          , orientation VERTICAL
          , weight 1.0
          ][ profilePictureLayout state push
           , profileOptionsLayout state push
           ]
        , BottomNavBar.view (push <<< BottomNavBarAction) (navData 4)
      ]
      , linearLayout 
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , background Color.lightBlack900
        , visibility if state.props.logoutModalView == true then VISIBLE else GONE
        ][ PopUpModal.view (push <<<PopUpModalAction) (logoutPopUp state) ]
    ]



------------------------------------------------- profilePictureLayout ------------------------------
profilePictureLayout :: ST.DriverProfileScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
profilePictureLayout state push =
    frameLayout
    [ width MATCH_PARENT
    , height $ V ((EHC.screenHeight unit)/3 - 10)
    ][ imageView
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , imageUrl "shape_profile_drawable"
      ]
      , linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , layoutGravity "center"
        , padding (PaddingTop 25)
        ][ imageView
            [ width $ V 75
            , height $ V 75
            , layoutGravity "center"
            , cornerRadius 45.0
            , id (EHC.getNewIDWithTag "ProfileImage")
            , imageWithFallback "ny_ic_profile_image,https://assets.juspay.in/nammayatri/images/common/ny_ic_profile_image.png"
            -- TODO : after 15 aug
            -- , afterRender (\action-> do
            --               _ <- pure $ JB.renderBase64Image state.data.base64Image (EHC.getNewIDWithTag "ProfileImage")
            --               pure unit)(const NoAction)
            ]
            , textView
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text (getValueToLocalStore USER_NAME)
            , layoutGravity "center"
            , color Color.black800
            , textSize FontSize.a_17
            , fontStyle $ FontStyle.medium LanguageStyle
            ]
            , textView
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text "Auto"--state.data.driverVehicleType
            , layoutGravity "center"
            , textSize FontSize.a_12
            , fontStyle $ FontStyle.regular LanguageStyle
            ]
            , ratingView state
        ]
    ]

------------------------------------------------- profileOptionsLayout ------------------------------

profileOptionsLayout :: ST.DriverProfileScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
profileOptionsLayout state push =
 scrollView
 [ width MATCH_PARENT
 , height WRAP_CONTENT
 ][ linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , padding (Padding 0 5 0 5)
    ] (map(\optionItem ->
            linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , gravity CENTER_VERTICAL
            , onClick push (const $ OptionClick optionItem.menuOptions)
            , visibility if (optionItem.menuOptions == DRIVER_BANK_DETAILS || optionItem.menuOptions == REFER) then GONE else VISIBLE
            ][ linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation HORIZONTAL
              , gravity CENTER_VERTICAL
              , padding (Padding 15 20 15 0)
              ][  imageView
                  [ width $ V 20
                  , height $ V 20
                  , imageWithFallback optionItem.icon
                  ]
                  , textView (
                  [ height WRAP_CONTENT
                  , weight 1.0
                  , text (getTitle optionItem.menuOptions)
                  , margin (MarginLeft 10)
                  , color Color.black900
                  ] <> FontStyle.subHeading2 TypoGraphy
                  )
                  , linearLayout
                  [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , orientation HORIZONTAL
                  , gravity CENTER_VERTICAL
                  ][ textView
                      [ width WRAP_CONTENT
                      , height WRAP_CONTENT
                      , text ("V " <> (getValueToLocalStore VERSION_NAME))
                      , textSize FontSize.a_14
                      , visibility if(optionItem.menuOptions == ABOUT_APP) then VISIBLE else GONE
                      , margin (MarginRight 5)
                      ]
                    , imageView
                      [ width $ V 18
                      , height $ V 18
                      , imageWithFallback "ny_ic_chevron_right_grey,https://assets.juspay.in/nammayatri/images/driver/ny_ic_chevron_right_grey.png"
                      ]
                  ]
              ]
              , if (optionItem.menuOptions == DRIVER_VEHICLE_DETAILS || optionItem.menuOptions == ABOUT_APP) then (horizontalLineView 7 0.5 0 20 0) else if(optionItem.menuOptions == DRIVER_LOGOUT) then dummyTextView else horizontalLineView 1 1.0 15 15 15
            ]
          ) (optionList "lazyEvaluation")
    )
 ]


--------------------------------------------------------------- ratingView ----------------------------
ratingView :: ST.DriverProfileScreenState -> forall w . PrestoDOM (Effect Unit) w
ratingView state= 
 linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , background Color.grey800
  , layoutGravity "center"
  , margin (MarginTop 7)
  , orientation HORIZONTAL
  , padding (Padding 13 6 13 6)
  , cornerRadius 5.0
  ][imageView
    [ width $ V 12
    , height MATCH_PARENT
    , imageWithFallback "ny_ic_star_active,https://assets.juspay.in/nammayatri/images/common/ny_ic_star_active.png"
    , gravity CENTER_VERTICAL
    ]
    , textView
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , text $ if (fromMaybe 0 state.data.driverRating ) == 0 then "New" else show (fromMaybe 0 state.data.driverRating )
    , margin (MarginLeft 7) 
    , textSize FontSize.a_14
    , color Color.black800
    ]
    , imageView
    [ width $ V 15
    , height MATCH_PARENT
    , imageWithFallback "ny_ic_chevron_right_grey,https://assets.juspay.in/nammayatri/images/driver/ny_ic_chevron_right_grey.png"
    , gravity CENTER_VERTICAL
    , margin (MarginLeft 5)
    , visibility GONE
    ]
  ]


--------------------------------------------------------------- horizontalLineView and dummyTextView ----------------------------
horizontalLineView :: Int -> Number -> Int -> Int -> Int -> forall w . PrestoDOM (Effect Unit) w
horizontalLineView heightOfLine lineAlpha marginLeft marginTop marginRight = 
 linearLayout
  [ width MATCH_PARENT
  , height $ V heightOfLine
  , background Color.grey800
  , margin (Margin marginLeft marginTop marginRight 0)
  , alpha lineAlpha
  ][]

dummyTextView :: forall w . PrestoDOM (Effect Unit) w
dummyTextView = 
 textView
 [ width WRAP_CONTENT
 , height WRAP_CONTENT
 ]
