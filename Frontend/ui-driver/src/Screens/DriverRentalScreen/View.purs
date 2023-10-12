module Screens.DriverRentalScreen.View where

import Prelude


import Common.Types.App
import Data.List
import Screens.DriverProfileScreen.ComponentConfig

import Animation as Anim
import Animation.Config as AnimConfig
import Animation.Config as AnimConfig
import Common.Types.App (LazyCheck(..))
import Components.BottomNavBar.Controller (navData)
import Components.BottomNavBar.View as BottomNavBar
import Components.CheckListView.View as CheckListView
import Components.CheckListView.View as CheckListView
import Components.GenericHeader.View as GenericHeader
import Components.GenericHeader.View as GenericHeader
import Components.InAppKeyboardModal.Controller as InAppKeyboardModalController
import Components.InAppKeyboardModal.Controller as InAppKeyboardModalController
import Components.InAppKeyboardModal.View as InAppKeyboardModal
import Components.InAppKeyboardModal.View as InAppKeyboardModal
import Components.PopUpModal as PopUpModal
import Components.PopUpModal as PopUpModal
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Components.PrimaryEditText as PrimaryEditText
import Components.PrimaryEditText.View as PrimaryEditText
import Components.PrimaryEditText.View as PrimaryEditText
import Control.Applicative (unless)
import Control.Applicative (unless)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array (length, mapWithIndex, null, any, (!!), take)
import Data.Either (Either(..))
import Data.List (elem)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Maybe (Maybe(..), isJust)
import Data.Maybe (fromMaybe)
import Debug (spy)
import Debug (spy)
import Debug (spy)
import Debug (spy)
import PrestoDOM.Properties as PP
import PrestoDOM.Types.DomAttributes as PTD
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (getNewIDWithTag, isPreviousVersion, liftFlow, screenWidth)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Utils as EHU
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import Helpers.Utils (getVehicleType)
import Helpers.Utils (getVehicleType)
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Utils (getValueFromConfig)
import MerchantConfig.Utils as MU
import Prelude (Unit, ($), const, map, (+), (==), (<), (||), (/), (/=), unit, bind, (-), (<>), (<=), (<<<), (>), pure, discard, show, (&&), void, negate, not)
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, alpha, background, clickable, color, cornerRadius, fontStyle, frameLayout, gravity, height, id, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, relativeLayout, scrollBarY, scrollView, text, textSize, textView, url, visibility, webView, weight, width)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), afterRender, alpha, background, clickable, color, ellipsize, fontSize, fontStyle, gravity, height, imageUrl, imageView, imageWithFallback, lineHeight, linearLayout, margin, maxLines, onClick, orientation, padding, relativeLayout, scrollView, singleLine, stroke, text, textSize, textView, visibility, weight, width, id, pivotY, onAnimationEnd)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), horizontalScrollView, afterRender, alpha, background, color, cornerRadius, fontStyle, frameLayout, gravity, height, id, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, scrollView, text, textSize, textView, visibility, weight, width, webView, url, clickable, relativeLayout, stroke, alignParentBottom, disableClickFeedback)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens as ScreenNames
import Screens as ScreenNames
import Screens.DriverRentalScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.DriverRentalScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Screens.Types as ST
import Services.API (GetDriverInfoReq(..), GetDriverInfoResp(..))
import Services.API (GetDriverInfoReq(..), GetDriverInfoResp(..), GetAllRcDataReq(..), GetAllRcDataResp(..))
import Services.Backend as Remote
import Services.Backend as Remote
import Storage (KeyStore(..), getValueToLocalStore)
import Storage (KeyStore(..), getValueToLocalStore)
import Storage (isLocalStageOn)
import Storage (isLocalStageOn)
import Styles.Colors as Color
import Styles.Colors as Color
import Types.App (defaultGlobalState)
import Screens.Types
import Data.Foldable (traverse_)
import Log (printLog)
import JBridge (getArray)

screen :: ST.DriverRentalScreenState -> Screen Action ST.DriverRentalScreenState ScreenOutput
screen initialState = 
    {
      initialState
    , view
    , name : "DriverRentalScreen"
    , globalEvents : [(\push -> do 
                _ <- pure $ printLog "initial State calling from driver rental screen"   initialState.data.rentalRequestDetails
                if true then
                  pure unit
                else do
                  pure unit
                pure (pure unit))]
    , eval : \action state -> do
        let _ = spy "DriverRentalScreen action " action
        let _ = spy "DriverProfileScreen State  update data" state.data.rentalRequestDetails
        eval action state
    }


view :: forall w. (Action -> Effect Unit) -> ST.DriverRentalScreenState -> PrestoDOM (Effect Unit) w
view push state =
  scrollView
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , scrollBarY true
    
    ][ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , onBackPressed push $ const BackPressed
        , background Color.greyLight
        
        ][
              headerView state push
             ,rentalRequestDetailsView push state.props.selectedRentalRequest state.props.isRentalAccepted
             ,renderRentalRequests push state
            
            
         ]

    ]

renderRentalRequests :: forall w. (Action -> Effect Unit) -> ST.DriverRentalScreenState ->PrestoDOM (Effect Unit) w
renderRentalRequests push  state = do
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL 
    , visibility if state.props.isRentalAccepted then GONE else VISIBLE
    ](mapWithIndex (\index item-> 
            if index /= state.props.selectedIndex then
                 rentalRequestShortView push item index state.props.isRentalAccepted
            else linearLayout[][]   
        ) state.data.rentalRequestDetails
        )
    
  
  

headerView :: forall w. ST.DriverRentalScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
headerView state push =
  linearLayout
  [  height WRAP_CONTENT
  ,  width MATCH_PARENT
  ,  orientation HORIZONTAL
  ,  gravity BOTTOM
  ,  background Color.white900
  ,  padding $ Padding 16 16 16 16
  ][ imageView
      [ width $ V 30
      , height $ V 30
      , imageWithFallback "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/driver/ny_ic_chevron_left.png"
      , onClick push $ const BackPressed
      ]
    , textView
      ([ weight 1.0
      ,  height MATCH_PARENT
      ,  text $ if state.props.isRentalAccepted then  "Rental Booking"else "Rental Requests" 
      ,  margin $ MarginLeft 20
      ,  color  Color.black900
      ] <> FontStyle.h3 TypoGraphy)
  ]

rentalRequestDetailsView :: forall w. (Action -> Effect Unit) -> RentalRequestDetial -> Boolean -> PrestoDOM (Effect Unit) w
rentalRequestDetailsView push state isRentalAccepted=
    linearLayout
    [  width MATCH_PARENT
    ,  height WRAP_CONTENT
    ,  orientation VERTICAL
    ,  background Color.white900
    ,  margin $ Margin 16 16 16 16
    , cornerRadii $ Corners 8.0 true true true true
    ][linearLayout[
        width MATCH_PARENT
       ,height WRAP_CONTENT
       ,orientation HORIZONTAL
       ,background Color.lightCyanColor
       ,padding $ Padding 12 12 12 12
     ][linearLayout[
        width WRAP_CONTENT
       ,height WRAP_CONTENT
       ,orientation HORIZONTAL
       ,background Color.purple
       ,cornerRadii $ Corners 28.0 true true true true
       
     ][imageView
      [ width $ V 16
      , height $ V 16
      , margin $ MarginTop 8
      , padding $ PaddingLeft 4
      , imageWithFallback $ "ic_tabler_disabled," <> (getAssetStoreLink FunctionCall) <> "ic_tabler_disabled.png"
      ],
        textView[
        text "Purple Ride"
       ,textSize FontSize.a_12
       ,color  Color.white900
       ,margin $ Margin 6 8 6 8
     ]]
     ,
     linearLayout[
        width WRAP_CONTENT
       ,height WRAP_CONTENT
       ,orientation HORIZONTAL
       ,background Color.blue800
       ,cornerRadii $ Corners 28.0 true true true true
       ,margin $ MarginLeft 8
     ][ imageView
      [ width $ V 16
      , height $ V 16
      , margin $ MarginTop 8
      , padding $ PaddingLeft 4
      , imageWithFallback $ "ic_bi_star_fill," <> (getAssetStoreLink FunctionCall) <> "ic_bi_star_fill.png"
      ],
        textView[
        text "Metro Ride"
       ,textSize FontSize.a_12
       ,color  Color.white900
       ,margin $ Margin 6 8 6 8
     ]
     ]
     ]
     ,linearLayout[
       width MATCH_PARENT
    ,  height WRAP_CONTENT
    ,  orientation VERTICAL
    ,  background Color.white900
    ,  padding $ Padding 16 16 16 16
     ][
      linearLayout
        [  width MATCH_PARENT
        ,  height WRAP_CONTENT
        ,  orientation HORIZONTAL
        ][ linearLayout
           [ width (V 0)
           , height WRAP_CONTENT
           , orientation HORIZONTAL
           , weight 1.0
           ][ textView
                ([ 
                   text ("Pickup")
                ,  textSize FontSize.a_16
                ,  color  Color.black700
                ,  fontStyle $ FontStyle.bold LanguageStyle
                ,  padding $ PaddingVertical 8 8
                ] )
              ,
              textView
                ([ 
                   text state.pickupDistance
                ,  textSize FontSize.a_22
                ,  color  Color.black900
                ,  margin $ MarginLeft 8
                ,  fontStyle $ FontStyle.bold LanguageStyle
                ])
           ]
           ,linearLayout
            [
              width (V 0)
            , weight 1.0
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            , gravity RIGHT
            ]
            [ imageView
                [ width (V 20)
                , height (V 20)
                , margin $ MarginTop 4
                , imageWithFallback $ "ic_clock," <> (getAssetStoreLink FunctionCall) <> "ic_clock.png"
                ]
                ,
                textView
                ([
                   text state.pickupTime
                ,  textSize FontSize.a_22
                ,  color  Color.black900
              
                ])

            ]
        ]
       ,linearLayout
        [ width MATCH_PARENT
        , margin $ Margin 10 0 10 0
        , height $ V 1
        , margin $ MarginTop 16
        , background Color.grey900
        ][]
        ,
        linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , margin $ MarginVertical 16 16
        ][  imageView
            [ height $ V 16
            , width $ V 16
            , imageWithFallback $ "ny_ic_source_dot," <> (getCommonAssetStoreLink FunctionCall) <> "/ny_ic_source_dot.png"
            , margin $ Margin 0 3 8 0
            , padding $ PaddingTop 4
            ]
        ,linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ][  textView $
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text state.sourceArea
            , color Color.black800
            , ellipsize true
            , singleLine true
          
            ] <> FontStyle.subHeading1 TypoGraphy
        , textView $
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text state.sourceAddress
            , color Color.black650
            ]<> FontStyle.body1 TypoGraphy
        , textView $
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text state.sourcePincode
            , color Color.black800
            , textSize FontSize.a_14
            , maxLines 2
           -- , maxLines if config.currentStage == RideAccepted || config.currentStage == ChatWithCustomer then 1 else 2
            ]<> FontStyle.body1 TypoGraphy
        ,   
        linearLayout
        [
            height WRAP_CONTENT
          , width WRAP_CONTENT
          , orientation HORIZONTAL
          , margin $ MarginTop 16
        ]
        [
          textView  [
            height WRAP_CONTENT
          , width WRAP_CONTENT
          , padding $ Padding 6 8 6 8
          , background Color.grey700
          , text state.time
          , color Color.black900
          , cornerRadii $ Corners 4.0 true true true true
          , stroke $ "1," <> Color.grey900
          , fontStyle $ FontStyle.bold LanguageStyle
          ]
          ,
           textView [
             margin $ MarginLeft 12
           , height WRAP_CONTENT
           , width WRAP_CONTENT
           , padding $ Padding 6 8 6 8
           , background Color.grey700
           , text state.distance
           , color Color.black900
           , cornerRadii $ Corners 4.0 true true true true
           , stroke $ "1," <> Color.grey900
           , fontStyle $ FontStyle.bold LanguageStyle
          ]
        ]
        ]
        ]
       ,linearLayout
        [ height $ (V 48)
        , width MATCH_PARENT
        , cornerRadii $ Corners 8.0 true true true true
        , gravity CENTER
        , background Color.grey700
        ]
        [  textView [
             text state.baseFare
           , color Color.black900
           , textSize FontSize.a_22
           , fontStyle $ FontStyle.bold LanguageStyle
          ]
        ]
        ,linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , background Color.white900
        , margin $ MarginTop 8
        ]
        [ 
        textView [
          text "Includes ₹10 Pickup. Extra per km charge: ₹12"
        , color Color.black700
        , textSize FontSize.a_12
        ]
        ]
        , linearLayout
        [ width MATCH_PARENT
         ,height WRAP_CONTENT
         ,orientation HORIZONTAL
         ,margin $ MarginTop 24
        ][
            linearLayout[
                width (V 0)
              , height WRAP_CONTENT
              , background Color.white900
              , cornerRadii $ Corners 8.0 true true true true
              , stroke $ "1," <> Color.grey900
              , weight 1.0 
              , gravity CENTER
              , visibility if isRentalAccepted then GONE else VISIBLE
            ][
                textView [
                  text "DECLINE"
                , gravity CENTER
                , color Color.black900
                , textSize FontSize.a_16
                , padding $ Padding 18 16 18 16
            ]
            ],
         linearLayout[
                width (V 0)
              , height WRAP_CONTENT
              , background Color.lightGradientPurple
              , cornerRadii $ Corners 8.0 true true true true
              , margin $ MarginLeft 12
              , weight 2.0 
              , gravity CENTER
              , onClick push $ const NAVIGATE_TO_PICKUP
            ][
                textView [
                  text   if isRentalAccepted then  "Navigate to Pickup" else "Accept Rental" 
                , color Color.black900
                , padding $ Padding 18 16 18 16
                , textSize FontSize.a_18
                , color Color.white900
            ]
            
            ]

        ]
     ]

    ]


rentalRequestShortView :: forall w.  (Action -> Effect Unit) ->  RentalRequestDetial -> Int -> Boolean -> PrestoDOM (Effect Unit) w
rentalRequestShortView push state selectedIndex isRentalPickuped = 
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , background Color.white900
    , margin $ Margin 16 16 16 0
    , padding $ Padding 8 16 8 16
    , cornerRadii $ Corners 8.0 true true true true
    , onClick push $ const (SELECT_RENTAL_REQUEST selectedIndex state)
    ][
      linearLayout[
        width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
    ][
        textView[
            height WRAP_CONTENT
          , width WRAP_CONTENT
          , text  state.sourceArea
          , textSize FontSize.a_14
          , color Color.black800
        ]
        ,
        imageView
        [ height $ V 4
        , width $ V 4
        , imageWithFallback $ "ic_elipse," <> (getCommonAssetStoreLink FunctionCall) <> "ic_elipse.png"
        , margin $ Margin 8 8 8 8
        ]
       ,textView[
            height WRAP_CONTENT
          , width WRAP_CONTENT
          , text state.distance 
          , textSize FontSize.a_14
          , color Color.black800
        ]
        ,
        textView[
            height WRAP_CONTENT
          , width WRAP_CONTENT
          , text state.baseFare
          , textSize FontSize.a_16
          , color Color.black800
          , weight 1.0
          , margin $ MarginRight 0
          , gravity RIGHT
        ]
    ]
    ,
    linearLayout[
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation VERTICAL
    ][ textView[
        text state.sourceAddress
      , textSize FontSize.a_14
      , color Color.black800
      , ellipsize true
      , singleLine true
    ]

    ]
    ]



