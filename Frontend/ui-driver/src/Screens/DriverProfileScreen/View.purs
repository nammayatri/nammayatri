{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DriverProfileScreen.View where

import Effect.Aff (launchAff)
import Common.Types.App
import Screens.DriverProfileScreen.ComponentConfig
import Engineering.Helpers.Commons (getNewIDWithTag, isPreviousVersion)

import Animation as Anim
import Components.BottomNavBar.Controller (navData)
import Components.BottomNavBar.View as BottomNavBar
import Components.PopUpModal as PopUpModal
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array (length, mapWithIndex, null)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, ($), const, map, (==), (||), (/), unit, bind, (-), (<>), (<<<), pure, discard, show, (&&), void)
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, alpha, background, color, cornerRadius, fontStyle, frameLayout, gravity, height, id, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, scrollView, text, textSize, textView, visibility, weight, width, webView, url, clickable)
import Screens.DriverProfileScreen.Controller (Action(..), ScreenOutput, eval, getTitle, optionList)
import Screens.DriverProfileScreen.ScreenData (MenuOptions(..))
import Screens.Types as ST
import Services.APITypes (GetDriverInfoReq(..), GetDriverInfoResp(..))
import Services.Backend as Remote
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color
import Screens as ScreenNames
import Helpers.Utils (getVehicleType)
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import Common.Types.App (LazyCheck(..))
import Merchant.Utils (getMerchant, Merchant(..))


screen :: ST.DriverProfileScreenState -> Screen Action ST.DriverProfileScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "DriverProfileScreen"
  , globalEvents : [(\push -> do
      void $ launchAff $ EHC.flowRunner $ runExceptT $ runBackT $ do
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
      , background Color.white900
      ][ Anim.screenAnimationFadeInOut $
          linearLayout
          [ width MATCH_PARENT
          , height MATCH_PARENT
          , orientation VERTICAL
          , weight 1.0
          ][ profilePictureLayout state push
           , profileOptionsLayout state push
           ]
        , BottomNavBar.view (push <<< BottomNavBarAction) (navData ScreenNames.DRIVER_PROFILE_SCREEN)
      ]
      , linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , background Color.lightBlack900
        , visibility if state.props.logoutModalView == true then VISIBLE else GONE
        ][ PopUpModal.view (push <<<PopUpModalAction) (logoutPopUp state) ]
      , if state.props.showLiveDashboard then showLiveStatsDashboard push state else dummyTextView
    ]


showLiveStatsDashboard :: forall w. (Action -> Effect Unit) -> ST.DriverProfileScreenState -> PrestoDOM (Effect Unit) w
showLiveStatsDashboard push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.grey800
  , afterRender
        ( \action -> do
            JB.initialWebViewSetUp push (getNewIDWithTag "webview") HideLiveDashboard
            pure unit
        )
        (const NoAction)
  ] [ webView
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , id (getNewIDWithTag "webview")
      , url if (isPreviousVersion (getValueToLocalStore VERSION_NAME) ("1.2.8")) then "https://nammayatri.in/open/" else "https://nammayatri.in/open?source=in-app"
      ]]

------------------------------------------------- profilePictureLayout ------------------------------
profilePictureLayout :: ST.DriverProfileScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
profilePictureLayout state push =
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , background Color.black900
    , padding $ Padding 16 40 16 24
    , gravity CENTER_VERTICAL
    ][ imageView
        [ width $ V 64
        , height MATCH_PARENT
        , layoutGravity "center"
        , cornerRadius 45.0
        , id $ EHC.getNewIDWithTag "ProfileImage"
        , imageWithFallback $ "ny_ic_profile_image," <> (getCommonAssetStoreLink FunctionCall) <> "/ny_ic_profile_image.png"
        ]
      , linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , padding $ PaddingLeft 20
        ][ textView $ 
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text $ getValueToLocalStore USER_NAME
            , color Color.white900
            ] <> FontStyle.h3 TypoGraphy
            , textView $
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , margin $ MarginTop 3
            , text $ getVehicleType state.data.driverVehicleType
            , color Color.black500
            ] <> FontStyle.captions TypoGraphy
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
    , padding $ Padding 0 10 0 5
    ] (mapWithIndex
        (\index optionItem ->
            linearLayout
            ([ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , gravity CENTER_VERTICAL
            , onClick push $ const $ OptionClick optionItem.menuOptions
            ] <> if (optionItem.menuOptions == DRIVER_BOOKING_OPTIONS) && (null state.data.downgradeOptions) then [alpha 0.5
            ,clickable false] else [])[ linearLayout
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
                  , text $ getTitle optionItem.menuOptions
                  , margin $ MarginLeft 10
                  , color Color.black900
                  ] <> FontStyle.subHeading2 TypoGraphy
                  )
                  , linearLayout
                  [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , orientation HORIZONTAL
                  , gravity CENTER_VERTICAL
                  ][ textView $
                      [ width WRAP_CONTENT
                      , height WRAP_CONTENT
                      , text $ "V " <> (getValueToLocalStore VERSION_NAME)
                      , visibility if(optionItem.menuOptions == ABOUT_APP) then VISIBLE else GONE
                      , margin (MarginRight 5)
                      ] <> FontStyle.paragraphText TypoGraphy
                    , imageView
                      [ width $ V 18
                      , height $ V 18
                      , imageWithFallback $ "ny_ic_chevron_right_grey," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_chevron_right_grey.png"
                      ]
                  ]
              ]
              , if (index == 2 || index == length (optionList "lazyEvaluation") - 2) then (horizontalLineView 7 0.5 0 20 0) else if(optionItem.menuOptions == DRIVER_LOGOUT) then dummyTextView else horizontalLineView 1 1.0 15 15 15
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
  , margin $ MarginTop 5
  ][imageView
    [ width $ V 12
    , height MATCH_PARENT
    , imageWithFallback $ "ny_ic_star_active," <> (getCommonAssetStoreLink FunctionCall) <> "/ny_ic_star_active.png"
    , gravity CENTER_VERTICAL
    ]
    , textView $
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , text $ if (fromMaybe 0 state.data.driverRating ) == 0 then "New" else show (fromMaybe 0 state.data.driverRating )
    , margin (MarginLeft 7)
    , color Color.white900
    ] <> FontStyle.paragraphText TypoGraphy
    , imageView
    [ width $ V 15
    , height MATCH_PARENT
    , imageWithFallback $ "ny_ic_chevron_right_grey," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_chevron_right_grey.png"
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