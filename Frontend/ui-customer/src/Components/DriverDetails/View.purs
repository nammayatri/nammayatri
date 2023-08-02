module Components.DriverDetails.View where

import Components.DriverDetails.Controller (Action (..),DriverDetailsState)
import Screens.Types (Stage(..), ZoneType(..))
import Font.Size as FontSize
import Font.Style as FontStyle
import Data.Array as Array
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split, length, take, drop)
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


view :: forall w. (Action ->Effect Unit) ->DriverDetailsState ->PrestoDOM( Effect Unit) w
view push state =
    linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , background Color.transparent
        , orientation VERTICAL
        ][ 
            -- driverInfoView push state
         summaryView push state


        ]

summaryView :: forall w. (Action -> Effect Unit) -> DriverDetailsState -> PrestoDOM( Effect Unit) w
summaryView push state =
    linearLayout
    [ orientation HORIZONTAL
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
        , height MATCH_PARENT
        , width WRAP_CONTENT
        ]
       ,linearLayout
        [ orientation VERTICAL
        , width $ V 158 
        , height WRAP_CONTENT
        , background Color.blue600
        , cornerRadius 10.0
        , padding $ Padding 16 16 16 16
        ]
        [
        textView
        [ text $ if state.data.rating ==0.0 then "New" else show state.data.rating
        , textSize FontSize.a_20
        , color Color.black600
        , fontStyle $ FontStyle.fontByOS "PlusJakartaSans-Bold" "PlusJakartaSans-Bold" "Arial"
        , lineHeight "25"
        , gravity LEFT
        , padding $ Padding 16 16 16 16
        ]
        ,imageView
          [ imageWithFallback "ny_ic_star_active,https://assets.juspay.in/nammayatri/images/common/ny_ic_star_active.png"
          , height $ V 13
          , width $ V 13
          ]

        ,textView
        [ text $ "rated by " <> show  state.data.totalUsersRated <> "users"
        , textSize FontSize.a_12
        , color Color.black700
        , fontStyle $ FontStyle.fontByOS "PlusJakartaSans-Bold" "PlusJakartaSans-Bold" "Arial"
        , lineHeight "16"
        , gravity BOTTOM
        , height WRAP_CONTENT
        , width WRAP_CONTENT 
        ]
      ]
      ,linearLayout
       [ orientation VERTICAL
       , width $ V 158
       , height WRAP_CONTENT
       , cornerRadius 10.0
       , background Color.blue600
       , padding $ Padding 16 16 16 16
       ]
        [
        textView
        [text $ show state.data.totalCompletedTrips
        , textSize FontSize.a_20
        , color Color.black600
        , fontStyle $ FontStyle.fontByOS "PlusJakartaSans-Bold" "PlusJakartaSans-Bold" "Arial"
        , lineHeight "25"
        , gravity LEFT
        , padding $ Padding 16 16 16 16
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
      ,linearLayout 
      [ orientation HORIZONTAL
      , width WRAP_CONTENT
      , height WRAP_CONTENT
      , cornerRadius 18.0
      , padding $ Padding 10 12 10 12
      ][
        textView
          [
            text $ show state.data.lateNightTrips 
            , textSize FontSize.a_14
            , color Color.black800
            , fontStyle $ FontStyle.fontByOS "PlusJakartaSans-Bold" "PlusJakartaSans-Bold" "Arial"
            , lineHeight "16"
            , gravity LEFT
            , height WRAP_CONTENT
            , width WRAP_CONTENT

          ]
          ,textView
            [ text $ "Late Night Trips"
            , textSize FontSize.a_12
            , color Color.black800
            , fontStyle $ FontStyle.fontByOS "PlusJakartaSans-Bold" "PlusJakartaSans-Bold" "Arial"
            , lineHeight "16"
            , height WRAP_CONTENT
            , width WRAP_CONTENT 
            ]

        ]
         ,linearLayout 
            [ orientation HORIZONTAL
            , width WRAP_CONTENT
            , height WRAP_CONTENT
            , cornerRadius 18.0
            , padding $ Padding 10 12 10 12
            ]
            [
            textView
            [
                  text $  state.data.lastRegistered
                , textSize FontSize.a_14
                , color Color.black800
                , fontStyle $ FontStyle.fontByOS "PlusJakartaSans-Bold" "PlusJakartaSans-Bold" "Arial"
                , lineHeight "16"
                , gravity LEFT
                , height WRAP_CONTENT
                , width WRAP_CONTENT

            ]
            ,textView
                [ text $ "on Namma Yatri"
                , textSize FontSize.a_12
                , color Color.black800
                 , fontStyle $ FontStyle.fontByOS "PlusJakartaSans-Bold" "PlusJakartaSans-Bold" "Arial"
                , lineHeight "16"
                , height WRAP_CONTENT
                , width WRAP_CONTENT 
                ]

        ]

    ]