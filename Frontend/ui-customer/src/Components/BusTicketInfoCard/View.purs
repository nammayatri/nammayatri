{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.BusTicketInfoCard.View where

import Common.Types.App
import Animation (fadeIn, fadeInWithDelay)
import Common.Types.App (LazyCheck(..))
import Components.BusTicketInfoCard.Controller
import Components.PrimaryButton as PrimaryButton
import Components.SourceToDestination as SourceToDestination
import Data.Array as Array
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split, length, take, drop, replaceAll, Replacement(..), contains, toLower)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (flowRunner, os, safeMarginBottom, screenWidth, getExpiryTime)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getAssetsBaseUrl, getPaymentMethod, secondsToHms, zoneOtpExpiryTimer, makeNumber, getVariantRideType, fetchImage, FetchImageFrom(..))
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Utils (Merchant(..), getMerchant, getValueFromConfig)
import Prelude (Unit, (<<<), ($), (/), (<>), (==), unit, show, const, map, (>), (-), (*), bind, pure, discard, not, (&&), (||), (/=))
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), Accessiblity(..), afterRender, alignParentBottom, alignParentLeft, alpha, background, clickable, color, cornerRadius, ellipsize, fontSize, fontStyle, frameLayout, gravity, height, imageUrl, imageView, imageWithFallback, letterSpacing, lineHeight, linearLayout, margin, maxLines, onClick, orientation, padding, scrollBarY, scrollView, singleLine, stroke, text, textSize, textView, visibility, weight, width, layoutGravity, accessibilityHint, accessibility, onAnimationEnd)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types (Stage(..), ZoneType(..), SearchResultType(..))
import Storage (isLocalStageOn, getValueToLocalStore)
import Styles.Colors as Color
import Common.Styles.Colors as CommonColor
import Storage (KeyStore(..))
import Data.Maybe (Maybe(..))
import Engineering.Helpers.Utils (showAndHideLoader)
import Types.App (defaultGlobalState)
import JBridge(fromMetersToKm)
import Engineering.Helpers.Suggestions (getMessageFromKey)

view :: forall w. (Action -> Effect Unit) -> BusTicketInfoCardState -> PrestoDOM ( Effect Unit ) w
view push state = 
    linearLayout
    [ height WRAP_CONTENT
    , width $ V (screenWidth unit)
    , background Color.grey700
    , orientation VERTICAL
    ]
    [ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity RIGHT
      , orientation VERTICAL
      , margin $ Margin 12 0 12 0
      ]
      [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ Margin 0 12 0 0
        , padding $ Padding 16 8 16 8
        ]
        [ textView $
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text $ "Bus is expected in "
          , color Color.black900
          ] 
          <> FontStyle.body7 TypoGraphy
        , textView $ 
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , margin $ Margin 0 0 4 0
          , text $ "2 mins" -- TODO: Replace with actual time
          , color Color.green900
          ]
          <> FontStyle.body7 TypoGraphy
        ]
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ Margin 0 12 0 0
        , background Color.white900
        , cornerRadius 8.0
        , padding $ Padding 16 0 16 0
        ]
        [ linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , padding $ Padding 16 12 20 12
          , gravity CENTER
          , clickable true
          , onClick push $ const ShowTicketQR
          ]
          [ imageView $
            [ height $ V 24
            , width $ V 24
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_referral_qr"
            , margin $ Margin 0 0 8 0
            , gravity CENTER_VERTICAL
            ]
          , textView $ 
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text $ "View Ticket QR"
            , color Color.blue900
            ]
            <> FontStyle.body4 TypoGraphy
          ]
        ]
      , linearLayout 
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ Margin 0 12 0 12
        , padding $ Padding 0 16 0 20
        , cornerRadius 8.0
        , background Color.white900
        ]
        [ linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          ]
          [ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , padding $ Padding 16 4 16 4
            , orientation HORIZONTAL
            ]
            [ linearLayout 
              [ gravity LEFT
              , height MATCH_PARENT
              , width WRAP_CONTENT
              , orientation VERTICAL
              , weight 1.0
              ] 
              [ linearLayout 
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                , weight 1.0
                ]
                [ linearLayout 
                  [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , orientation HORIZONTAL
                  , gravity CENTER
                  ] 
                  [ textView $ 
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , text $ state.data.busId
                    , color Color.blueMagenta
                    , padding $ Padding 8 2 8 2
                    , background Color.purple100
                    , cornerRadius 4.0
                    ] <> FontStyle.body15 TypoGraphy
                  , linearLayout
                    [ height $ V 4
                    , width $ V 4
                    , cornerRadius 2.5
                    , background Color.blueMagenta
                    , gravity CENTER_VERTICAL
                    , margin $ Margin 6 0 6 0
                    ][]
                  , textView $ 
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , text $ show state.data.quantity <> " tickets"
                    , color Color.blueMagenta
                    , padding $ Padding 0 2 0 2
                    ] <> FontStyle.body1 TypoGraphy
                  ]
                , linearLayout
                  [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , margin $ Margin 0 4 0 0
                  ]
                  [ textView $ 
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , text $ "View Route"
                    , color Color.blue900
                    , clickable true
                    , onClick push $ const ShowRouteInfo
                    ]
                    <> FontStyle.body3 TypoGraphy
                  ]
                ]
              , linearLayout 
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity BOTTOM
                , orientation VERTICAL
                ] 
                [ textView $ 
                  [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , text $ state.data.routeNo
                  , color Color.black800
                  ] <> FontStyle.body9 TypoGraphy
                , textView $ 
                  [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , text $ state.data.busModel
                  , color Color.black700
                  ] <> FontStyle.captions TypoGraphy
                ]
            ]
          , linearLayout 
              [ gravity RIGHT
              , height WRAP_CONTENT
              , width WRAP_CONTENT
              , orientation VERTICAL
              ] 
              [ imageView $ 
                  [ height $ V 70
                  , width $ V 123
                  , imageWithFallback $ fetchImage FF_ASSET "ny_ic_bus"
                  ]
                , linearLayout 
                  [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , orientation HORIZONTAL
                  , stroke $ "2," <> Color.black
                  , background Color.yellow900
                  ]
                  [ imageView
                    [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_number_plate"
                    , gravity LEFT
                    , background "#1C4188"
                    , height $ V 28
                    , width $ V 19
                    ]
                  , textView $
                    [ margin $ Margin 2 2 2 2
                    , weight 1.0
                    , height MATCH_PARENT
                    , text $ (makeNumber state.data.registrationNumber)
                    , color Color.black
                    , gravity CENTER
                    ] <> FontStyle.body7 TypoGraphy
                  ]
              ]  
            ]
          ]
        ]
      ]
    ]