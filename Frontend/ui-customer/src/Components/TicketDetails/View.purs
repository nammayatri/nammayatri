{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.TicketDetails.View where

import Common.Types.App
import Animation (fadeIn, fadeInWithDelay)
import Common.Types.App (LazyCheck(..))
import Components.TicketDetails.Controller
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
import Engineering.Helpers.Commons (flowRunner, os, safeMarginBottom, screenWidth, getExpiryTime, getNewIDWithTag)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getAssetsBaseUrl, getPaymentMethod, secondsToHms, zoneOtpExpiryTimer, makeNumber, getVariantRideType, fetchImage, FetchImageFrom(..))
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Utils (Merchant(..), getMerchant, getValueFromConfig)
import Prelude (Unit, (<<<), ($), (/), (<>), (==), unit, show, const, map, (>), (-), (*), bind, pure, discard, not, (&&), (||), (/=))
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), Accessiblity(..), afterRender, alignParentBottom, alignParentLeft, alpha, background, clickable, color, cornerRadius, ellipsize, fontSize, fontStyle, frameLayout, gravity, height, imageUrl, imageView, imageWithFallback, letterSpacing, lineHeight, linearLayout, margin, maxLines, onClick, orientation, padding, scrollBarY, scrollView, singleLine, stroke, text, textSize, textView, visibility, weight, width, layoutGravity, accessibilityHint, accessibility, onAnimationEnd, id)
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

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config = 
    linearLayout 
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.grey700
    , orientation VERTICAL
    ] 
    [ linearLayout
      [ height $ V 59
      , width MATCH_PARENT
      , gravity CENTER_VERTICAL
      , background Color.white900
      , padding $ Padding 16 16 16 16
      , margin $ Margin 0 0 0 24
      , orientation HORIZONTAL
      ] 
      [ imageView
        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_left"
        , height $ V 27
        , width $ V 24
        , margin $ Margin 0 0 8 0
        , gravity CENTER_VERTICAL
        , clickable true
        , onClick push $ const CloseTicketDetails
        ]
      , textView $
        [ text "Ticket Details"
        , width WRAP_CONTENT
        , height WRAP_CONTENT
        , gravity CENTER_VERTICAL
        , color Color.black800
        ] <> FontStyle.subHeading1 TypoGraphy
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , background Color.white900
      , padding $ Padding 16 16 16 16 
      , margin $ Margin 16 16 16 16
      , gravity CENTER
      , orientation VERTICAL
      ] 
      [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        ]
        [ imageView 
          [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_left"
          , height $ V 32
          , width $ V 32
          , gravity LEFT
          ]
        , textView $ 
          [ width WRAP_CONTENT
          , height MATCH_PARENT
          , text $ "Ticket"
          , color Color.black800
          , gravity CENTER
          , weight 1.0
          ] <> FontStyle.subHeading1 TypoGraphy
        , imageView 
          [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_right"
          , height $ V 32
          , width $ V 32
          , gravity RIGHT
          ]
        ] -- Ticket No and left right button
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , gravity CENTER
        , margin $ Margin 0 16 0 16
        , padding $ Padding 0 8 0 8
        ]
        [ imageView 
          [ width $ V 218
          , height  $ V 218
          , gravity CENTER_HORIZONTAL
          , id $ getNewIDWithTag config.ticketQR
          , afterRender push (const (TicketQrRendered config.ticketQR (getNewIDWithTag config.ticketQR)))
          ]
        , linearLayout 
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , margin $ Margin 0 8 0 0
          , weight 1.0
          , gravity CENTER
          ]
          [ imageView
            [ width $ V 137
            , height $ V 42
            , gravity CENTER
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_share_ticket"
            ]
          ]
        ] -- QR Code with share button
      , textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , gravity CENTER
        , text $ "Valid till " <> config.validTill
        , color Color.black800
        ] <> FontStyle.body1 TypoGraphy -- Valid Till
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , margin $ Margin 16 0 16 0
      , padding $ Padding 16 16 16 16
      , background Color.white900
      , orientation VERTICAL
      ] 
      [ linearLayout 
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        ] 
        [ textView $ 
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text $ config.busId
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
          , text $ show config.quantity <> " tickets"
          , color Color.blueMagenta
          , padding $ Padding 0 2 0 2
          ] <> FontStyle.body1 TypoGraphy
        ] -- quantity and busId
      , linearLayout 
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , margin $ Margin 0 12 0 12
        ] 
        [ linearLayout 
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , orientation HORIZONTAL
          , gravity CENTER
          , margin $ Margin 0 0 0 4
          ] 
          [ imageView 
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_pickup"
            , height $ V 8
            , width $ V 8
            , margin $ Margin 0 0 6 0
            , gravity CENTER_VERTICAL
            ]
          , textView $ 
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text "Boarding at"
            , color Color.black700
            ] <> FontStyle.body3 TypoGraphy
          ]
        , textView $
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text $ config.start.location.place
          , color "#454545"
          ] <> FontStyle.body1 TypoGraphy
        ] -- Source
      , linearLayout 
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , margin $ Margin 0 12 0 12
        ] 
        [ linearLayout 
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , orientation HORIZONTAL
          , gravity CENTER
          , margin $ Margin 0 0 0 4
          ] 
          [ imageView 
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_drop"
            , height $ V 8
            , width $ V 8
            , margin $ Margin 0 0 6 0
            , gravity CENTER_VERTICAL
            ]
          , textView $ 
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text "Dropping off at"
            , color Color.black700
            ] <> FontStyle.body3 TypoGraphy
          ]
        , textView $
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text $ config.end.location.place
          , color "#454545"
          ] <> FontStyle.body1 TypoGraphy
        ] -- Destination
      , textView $ 
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text "View Route Details"
        , color Color.blue900
        , clickable true
        , onClick push $ const ShowRouteDetails
        ] <> FontStyle.body1 TypoGraphy
        -- View Route Details
      ]
    ]