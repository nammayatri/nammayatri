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
import Types.App (defaultGlobalState)
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
import Prelude (Unit, ($), const, map, (==), (||), (/), unit, bind, (-), (<>), (<<<), pure, discard, show, (&&), void, negate, not)
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, alpha, background, color, cornerRadius, fontStyle, frameLayout, gravity, height, id, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, scrollView, text, textSize, textView, visibility, weight, width, webView, url, clickable, relativeLayout)
import Screens.DriverProfileScreen.Controller (Action(..), ScreenOutput, eval, getTitle)
import Screens.DriverProfileScreen.ScreenData (MenuOptions(..), optionList)
import Screens.Types as ST
import Services.APITypes (GetDriverInfoReq(..), GetDriverInfoResp(..))
import Services.Backend as Remote
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color
import Screens as ScreenNames
import Helpers.Utils (getVehicleType)
import PrestoDOM.Animation as PrestoAnim
import Animation.Config as AnimConfig
import Data.Maybe (Maybe(..), isJust)


screen :: ST.DriverProfileScreenState -> Screen Action ST.DriverProfileScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "DriverProfileScreen"
  , globalEvents : [(\push -> do
      void $ launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT $ do
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
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , background Color.white900
  ][  headerView state push 
    , driverDetailsView state push

  ]


headerView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
headerView state push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation HORIZONTAL 
  , gravity CENTER_VERTICAL
  , padding $ Padding 16 16 16 16
  ][ imageView
      [ width $ V 30
      , height $ V 30
      , imageWithFallback "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/driver/ny_ic_chevron_left.png"
      -- , onClick push $ const BackPressed
      , padding $ Padding 2 2 2 2
      , margin $ MarginLeft 5
      ]
    , textView
      [ weight 1.0
      , height WRAP_CONTENT
      , text "My Profile"
      , fontStyle $ FontStyle.semiBold LanguageStyle
      , textSize FontSize.a_18
      , margin $ MarginLeft 20
      , color Color.black900
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , gravity CENTER_VERTICAL
      ][  imageView
          [ height $ V 14
          , width $ V 14 
          , margin $ MarginRight 4
          , imageWithFallback "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/driver/ny_ic_chevron_left.png"
          ]
        , textView
          [ text "Settings"
          , textSize FontSize.a_14 
          , color Color.blue900
          , fontStyle $ FontStyle.semiBold LanguageStyle
          ]
      ]
    

  ]

driverDetailsView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
driverDetailsView state push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , background Color.blue600
  , orientation VERTICAL
  , padding $ PaddingVertical 16 24
  ][  tabView state push
    , tabImageView state push  
    , driverDetails state push 
    ]

tabView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
tabView state push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , cornerRadius 24.0 
  , margin $ MarginHorizontal 16 16
  , background Color.white900
  , padding $ Padding 6 6 6 6
  , gravity CENTER
  ][  
      textView
      [ height WRAP_CONTENT
      , weight 1.0 
      , background if state.props.screenType == ST.DRIVER_DETAILS then Color.black900 else Color.white900
      , text "Driver Details"
      , cornerRadius 24.0 
      , padding $ PaddingVertical 6 6
      , onClick push $ const $ ChangeScreen ST.DRIVER_DETAILS
      , fontStyle $ FontStyle.medium LanguageStyle
      , gravity CENTER
      , color if state.props.screenType == ST.DRIVER_DETAILS then Color.white900 else Color.black900
      ]
    , textView
      [ height WRAP_CONTENT
      , weight 1.0 
      , gravity CENTER
      , cornerRadius 24.0 
      , onClick push $ const $ ChangeScreen ST.AUTO_DETAILS
      , padding $ PaddingVertical 6 6
      , text "Auto Details"
      , fontStyle $ FontStyle.medium LanguageStyle
      , background if state.props.screenType == ST.AUTO_DETAILS then Color.black900 else Color.white900
      , color if state.props.screenType == ST.AUTO_DETAILS then Color.white900 else Color.black900
      ]
  ]
  -- frameLayout
  --   [ width MATCH_PARENT
  --   , height MATCH_PARENT
  --   ][ linearLayout
  --     [ height MATCH_PARENT
  --     , width MATCH_PARENT
  --     , orientation VERTICAL
  --     , background Color.white900
  --     , onBackPressed push (const BackPressed state)
  --     , afterRender push (const AfterRender)
  --     , background Color.white900
  --     ][ Anim.screenAnimationFadeInOut $
  --         linearLayout
  --         [ width MATCH_PARENT
  --         , height MATCH_PARENT
  --         , orientation VERTICAL
  --         , weight 1.0
  --         ][ profilePictureLayout state push
  --          , profileOptionsLayout state push
  --          ]
  --       , BottomNavBar.view (push <<< BottomNavBarAction) (navData ScreenNames.DRIVER_PROFILE_SCREEN)
  --     ]
  --     , linearLayout
  --       [ width MATCH_PARENT
  --       , height MATCH_PARENT
  --       , background Color.lightBlack900
  --       , visibility if state.props.logoutModalView == true then VISIBLE else GONE
  --       ][ PopUpModal.view (push <<<PopUpModalAction) (logoutPopUp state) ]
  --     , if state.props.showLiveDashboard then showLiveStatsDashboard push state else dummyTextView
  --   ]


tabImageView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
tabImageView state push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER_HORIZONTAL
  , padding $ PaddingTop 32
  , orientation HORIZONTAL
  ][  PrestoAnim.animationSet [ Anim.scaleAnim $ autoAnimConfig (state.props.screenType == ST.DRIVER_DETAILS)
  , Anim.scaleAnim1 $ autoAnimConfig ( (state.props.screenType == ST.AUTO_DETAILS))] $ 
    imageView
      [height $ V 88
      , width $ V 88 
      , cornerRadius 44.0
      , margin $ MarginRight 10
      , onClick push $ const AfterRender
      , alpha if (state.props.screenType == ST.DRIVER_DETAILS) then 1.0 else 0.4
      , imageWithFallback "ny_ic_user,https://assets.juspay.in/nammayatri/images/user/ny_ic_user.png" --change the link once uploaded to asset
      ]
  ,  PrestoAnim.animationSet [ Anim.scaleAnim1 $ autoAnimConfig1 (state.props.screenType == ST.AUTO_DETAILS)
  , Anim.scaleAnim $ autoAnimConfig1 ( (state.props.screenType == ST.DRIVER_DETAILS))] $ 
   linearLayout
      [ height $ V 88
      , width $ V 88
      , cornerRadius 44.0
      , background Color.white900
      , onClick push $ const AfterRender
      , gravity CENTER
      , alpha if (state.props.screenType == ST.AUTO_DETAILS) then 1.0 else 0.4
      ][  imageView 
          [ imageWithFallback "ny_ic_auto_side_view,https://assets.juspay.in/nammayatri/images/common/ic_navigation_blue11.png" --change this image link after uploading in asset store
          , height $ V 68
          , width $ V 68
          ]
      ]

  ]


driverDetails :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
driverDetails state push = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , padding $ PaddingHorizontal 16 16
  , background Color.white900
  , margin $ Margin 16 32 16 0
  , orientation VERTICAL
  , cornerRadius 10.0
  ](map(\item ->  
        linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ][linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation HORIZONTAL
            , padding $ PaddingVertical 16 16
            ][  textView
                [ text item.key
                , textSize FontSize.a_12 
                , color Color.black700
                , fontStyle $ FontStyle.regular LanguageStyle
                ]
              , linearLayout
                [ height WRAP_CONTENT
                , weight 1.0
                ][]
              , textView
                [ text $ fromMaybe "" item.value
                , textSize FontSize.a_14
                , onClick push $ const item.action
                , color Color.black900
                , fontStyle $ FontStyle.semiBold LanguageStyle
                ]
            ]
          , linearLayout
            [ height $ V 1
            , width MATCH_PARENT
            , background Color.grey700
            ][]
            ]
        ) (driverDetailsArray state))


driverDetailsArray state = [
    { key : "Name" , value : Just "Rammoorthy Parashuram" , action : NoAction , isEditable : false }
  , { key : "Mobile Number" , value : Just "9988776655" , action : NoAction , isEditable : false }
  , { key : "Alternate Number" , value : Nothing , action : if (isJust state.data.driverAlternateNumber) then NoAction else UpdateValue "Alternate Number" , isEditable : true}
  , { key : "Gender" , value : Nothing , action : if (isJust state.data.gender) then NoAction else UpdateValue "Gender" , isEditable : true } ]

autoAnimConfig :: Boolean ->  AnimConfig.AnimConfig
autoAnimConfig state =
  let
    config = AnimConfig.animConfig
    autoAnimConfig' =
      config
        { duration = 10000
        , toScaleX = 1.0
        , toScaleY = 1.0
        , fromScaleY = 0.5
        , fromScaleX = 0.5
        , fromX = - 10
        , toX = 0
        , ifAnim = state
        }
  in
    autoAnimConfig'

autoAnimConfig1 :: Boolean ->  AnimConfig.AnimConfig
autoAnimConfig1 state =
  let
    config = AnimConfig.animConfig
    autoAnimConfig' =
      config
        { duration = 10000
        , toScaleX = 0.5
        , toScaleY = 0.5
        , fromScaleY = 1.0
        , fromScaleX = 1.0
        , fromX = - 10
        , toX = 0
        , ifAnim = state
        }
  in
    autoAnimConfig'


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
        , imageWithFallback "ny_ic_profile_image,https://assets.juspay.in/nammayatri/images/common/ny_ic_profile_image.png"
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
            , textView
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , margin $ MarginTop 3
            , text $ getVehicleType state.data.driverVehicleType
            , textSize FontSize.a_10
            , fontStyle $ FontStyle.regular LanguageStyle
            , color Color.black500
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
                  ][ textView
                      [ width WRAP_CONTENT
                      , height WRAP_CONTENT
                      , text $ "V " <> (getValueToLocalStore VERSION_NAME)
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
    , imageWithFallback "ny_ic_star_active,https://assets.juspay.in/nammayatri/images/common/ny_ic_star_active.png"
    , gravity CENTER_VERTICAL
    ]
    , textView
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , text $ if (fromMaybe 0 state.data.driverRating ) == 0 then "New" else show (fromMaybe 0 state.data.driverRating )
    , margin (MarginLeft 7)
    , textSize FontSize.a_14
    , color Color.white900
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