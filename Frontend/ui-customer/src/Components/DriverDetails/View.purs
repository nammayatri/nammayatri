module Components.DriverDetails.View where

import Components.DriverDetails.Controller (Action (..),DriverDetailsState)
import Components.DriverInfoCard as DriverInfoCard
import Screens.Types (Stage(..), ZoneType(..))
import Components.PrimaryButton as PrimaryButton
import Debug (spy)
import Font.Size as FontSize
import Font.Style as FontStyle
import Data.Array as Array
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split, length, take, drop)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Helpers.Utils (secondsToHms, zoneOtpExpiryTimer, Merchant(..), getMerchant, makeNumber)
import Storage (isLocalStageOn, getValueToLocalStore)
import Styles.Colors as Color
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), afterRender, alignParentBottom, alignParentLeft, alpha, background, clickable, color, cornerRadius, ellipsize, fontSize, fontStyle, frameLayout, gravity, height, imageUrl, imageView, imageWithFallback, letterSpacing, lineHeight, linearLayout, margin, maxLines, onClick, orientation, padding, scrollBarY, scrollView, singleLine, stroke, text, textSize, textView, visibility, weight, width)
import Merchant.Utils (getValueFromConfig)
import Prelude (Unit, (<<<), ($), (/), (<>), (==), unit, show, const, map, (>), (-), (*), bind, pure, discard, (&&), (||), (/=), not)
import Effect.Class (liftEffect)
import Effect (Effect)
import Language.Strings (getString)
import Language.Types (STR(..))
import Presto.Core.Types.Language.Flow (doAff)
import Engineering.Helpers.Commons (flowRunner, os, safeMarginBottom, screenWidth, getExpiryTime)
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import PrestoDOM.Animation as PrestoAnim
import Storage (KeyStore(..))
import Components.DriverInfoCard(mapOptionsView)


view :: forall w. (Action ->Effect Unit) ->DriverDetailsState ->PrestoDOM( Effect Unit) w
view push state =
    linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , background Color.transparent
        , orientation VERTICAL
        ]
        [ 
            --  mapOptionsView push state
        --  , messageNotificationView push state
         
         driverDetailsView push state


        ]


driverDetailsView :: forall w. (Action -> Effect Unit) -> DriverDetailsState -> PrestoDOM( Effect Unit) w
driverDetailsView push state = 
  linearLayout
  [ width MATCH_PARENT
  , height $ V 689
  , margin $ MarginTop 71
  ][ (if os == "IOS" then linearLayout else scrollView)
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , scrollBarY false
      ][ linearLayout
         [ orientation VERTICAL
         , height WRAP_CONTENT
         , width MATCH_PARENT
         , margin $ MarginTop 14
         , background Color.blue800
         , gravity CENTER
         , cornerRadii $ Corners 24.0 true true false false
         , stroke $ "1," <> Color.grey900
         ][ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , background Color.blue800
            , cornerRadii $ Corners 24.0 true true false false
            , gravity CENTER
            , orientation HORIZONTAL
            , padding (PaddingVertical 4 4)
            , visibility if state.props.zoneType == METRO then VISIBLE else GONE
            ][ imageView
                [ width (V 15)
                , height (V 15)
                , margin (MarginRight 6)
                , imageWithFallback "ny_ic_metro_white,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_metro_white.png"
                ]
              , textView
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , textSize FontSize.a_14
                , text (getString METRO_RIDE)
                , color Color.white900
                ]
             ]
          , linearLayout
            [ orientation VERTICAL
            , height WRAP_CONTENT
            , width MATCH_PARENT
            , padding $ PaddingBottom 24
            , background Color.white900
            , gravity CENTER
            , cornerRadii $ Corners 24.0 true true false false
            , stroke $ "1," <> Color.grey900
            ][ linearLayout
              [ gravity CENTER
              , background Color.transparentGrey
              , height $ V 4
              , width $ V 34
              , margin (MarginTop 8)
              , cornerRadius 4.0
              ][]
            --   , if state.props.isSpecialZone  then headerTextView push state else contactView push state
            --   , otpAndWaitView push state
              
              , summaryView push state
            --   , separator (MarginHorizontal 16 16) (V 1) Color.grey900 true
            --   , paymentMethodView push state (getString RIDE_FARE) true
              
            --   , (if os == "IOS" then scrollView else linearLayout)
            --     [ width MATCH_PARENT
            --     , height if os == "IOS" then (V 210) else WRAP_CONTENT
            --     , orientation VERTICAL
            --     ]
                -- ][ if state.props.isSpecialZone then destinationView push state else  sourceDistanceView push state
                  --, separator (Margin 0 0 0 0) (V 1) Color.grey900 (Array.any (_ == state.props.currentStage) [ RideAccepted, RideStarted, ChatWithDriver ])
                --   , cancelRideLayout push state
                ]
              ]
         ]
      ]
--   ]
summaryView :: forall w. (Action -> Effect Unit) -> DriverDetailsState -> PrestoDOM( Effect Unit) w
summaryView push state =
    linearLayout
    [ orientation VERTICAL
    , height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ Padding 12 12 12 12 
    ][
      textView
        [ text $ "Summary"
        , fontStyle $ FontStyle.fontByOS "PlusJakartaSans-Bold" "PlusJakartaSans-Bold" "Arial"
        , color Color.black
        , textSize FontSize.a_16
        , lineHeight "20"
        , height WRAP_CONTENT
        , width WRAP_CONTENT
        ]
        ,linearLayout
        [ orientation HORIZONTAL
        , width WRAP_CONTENT 
        , height MATCH_PARENT
        ][
         linearLayout
        [ orientation VERTICAL
        , width $ V 158
        , height $ V 77
        , background Color.blue600
        , cornerRadius 10.0
        , margin $ MarginTop  12 
         , padding $ Padding 16 0 16 0
        ]
        [ 
         textView
        [ text $  show state.data.rating
        , height WRAP_CONTENT   
        , width WRAP_CONTENT       
        , textSize FontSize.a_20
        , color Color.black800
        , fontStyle $ FontStyle.fontByOS "PlusJakartaSans-Bold" "PlusJakartaSans-Bold" "Arial"
        , lineHeight "25"
        , gravity LEFT
       
        ]
        ,imageView
          [ imageWithFallback "ny_ic_star_active,https://assets.juspay.in/nammayatri/images/common/ny_ic_star_active.png"
          , height $ V 13
          , width $ V 13
          ]

        ,textView
        [ text $ "rated by " <> show  state.data.totalUsersRated <> " users"
        , textSize FontSize.a_12
        , color Color.black700
        , fontStyle $ FontStyle.fontByOS "PlusJakartaSans-Bold" "PlusJakartaSans-Bold" "Arial"
        , lineHeight "16"
        , gravity BOTTOM
        , height $ V 16
        , width $ V 104
        
        ]
        ]
         ,linearLayout
        [ orientation VERTICAL
        , width $ V 158
        , height $ V 77
        , background Color.blue600
        , cornerRadius 10.0
        , margin $ Margin 12 12 12 12
        , padding $ Padding 16 16 0 0
        
        ]
      
        [
        textView
        [text $ show state.data.totalCompletedTrips
        , textSize FontSize.a_20
        , color Color.black800
        , fontStyle $ FontStyle.fontByOS "PlusJakartaSans-Bold" "PlusJakartaSans-Bold" "Arial"
        , lineHeight "25"
        , gravity LEFT
        
        ]
        ,textView
        [
          text $ "Trips Completed"
        , textSize FontSize.a_12
        , color Color.black700
         , fontStyle $ FontStyle.fontByOS "PlusJakartaSans-Bold" "PlusJakartaSans-Bold" "Arial"
        , lineHeight "16"
        , gravity BOTTOM
        , height WRAP_CONTENT
        , width WRAP_CONTENT 
        ]
        ]
    ]
    
        
      ,linearLayout 
      [ orientation HORIZONTAL
      , width $ V 328
      , height WRAP_CONTENT
      ][
         linearLayout
        [ width WRAP_CONTENT
        , height $ V 36
        , background Color.blue600
        , cornerRadius 18.0
        , padding $ Padding 10 8 10 10
        -- , padding $ Padding 10 12 10 12
        ][
        textView
          [
            text $ show state.data.lateNightTrips 
            , textSize FontSize.a_14
            , color Color.black800
            , fontStyle $ FontStyle.fontByOS "PlusJakartaSans-Bold" "PlusJakartaSans-Bold" "Arial"
            , lineHeight "16"
            , gravity CENTER
            , height WRAP_CONTENT
            , width WRAP_CONTENT

          ]
          ,textView
            [ text $ " Late Night Trips"
            , textSize FontSize.a_12
            , color Color.black800
            , fontStyle $ FontStyle.fontByOS "PlusJakartaSans-Bold" "PlusJakartaSans-Bold" "Arial"
            , lineHeight "16"
            , height WRAP_CONTENT
            , width WRAP_CONTENT 
            ]
        ]

        
         , linearLayout
        [ 
         width WRAP_CONTENT
        , height $ V 36
        , background Color.blue600
        , cornerRadius 18.0
        , padding $ Padding 10 8 10 0
        ]
            [
            textView
            [
                  text $ "2 Yrs"
                , textSize FontSize.a_14
                , color Color.black800
                , fontStyle $ FontStyle.fontByOS "PlusJakartaSans-Bold" "PlusJakartaSans-Bold" "Arial"
                , lineHeight "16"
                , gravity LEFT
                , height WRAP_CONTENT
                , width WRAP_CONTENT

            ]
            ,textView
                [ text $ " on Namma Yatri"
                , textSize FontSize.a_12
                , color Color.black800
                 , fontStyle $ FontStyle.fontByOS "PlusJakartaSans-Bold" "PlusJakartaSans-Bold" "Arial"
                , lineHeight "16"
                , height WRAP_CONTENT
                , width WRAP_CONTENT 
                ]

        ]

    ]
    ]
    
    

separator :: forall w. Margin -> Length -> String -> Boolean -> PrestoDOM (Effect Unit) w
separator margin' height' color' isVisible =
  linearLayout
  [ height $ height'
  , margin $ margin'
  , width MATCH_PARENT
  , visibility if isVisible then VISIBLE else GONE
  , background color'
  ][]