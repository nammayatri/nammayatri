module Components.SelectRouteButton where

import Prelude (Unit, const, ($), (<>))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, background, color, cornerRadius, ellipsize, gravity, height, imageView, imageWithFallback, layoutGravity, linearLayout, margin, maxLines, onClick, orientation, padding, singleLine, stroke, text, textFromHtml, textView, visibility, weight, width)
import Common.Types.App (LazyCheck(..))
import Helpers.Utils as HU
import Font.Style as FontStyle
import Styles.Colors as Colors
import Mobility.Prelude (boolToVisibility)
import Effect (Effect)
import Font.Style (Style)
import Styles.Types (Color)

data Action = NoAction | Click | Select Int String

type RouteDisplayConfig = {
    routeNumber :: String
  , routeNumberColor :: Color
  , sourceName :: String
  , destinationName :: String
  , onClick :: Action
  , showChevron :: Boolean
  , isSelectable :: Boolean
  , showDot :: Boolean
  , showRouteDetails :: Boolean
  , cornerRadius :: Number
  , margin :: Margin
  , padding :: Padding
  , useHtmlFormatting :: Boolean
  , fontSize :: Style
  , routeFontSize :: Style
}

defaultConfig :: RouteDisplayConfig
defaultConfig = {
    routeNumber: ""
  , routeNumberColor: Colors.black900
  , sourceName: ""
  , destinationName: ""
  , onClick: NoAction
  , showChevron: false
  , isSelectable: true
  , showDot: true
  , showRouteDetails: true
  , cornerRadius: 8.0
  , margin: Margin 0 0 0 0
  , padding: Padding 16 16 16 16
  , useHtmlFormatting: false
  , fontSize: FontStyle.Body16
  , routeFontSize : FontStyle.SubHeading3
}

view :: forall w. (Action -> Effect Unit) -> RouteDisplayConfig -> PrestoDOM (Effect Unit) w
view push config = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , padding config.padding
    , cornerRadius config.cornerRadius
    , margin config.margin
    , stroke $ "1," <> Colors.grey900
    , gravity CENTER_VERTICAL
    , background Colors.white900
    , onClick push $ const config.onClick
    ][
      routeNumberView config
    , dotSeparator config
    , routeDetailsView config
    , chevronView config
    ]

routeNumberView :: forall w. RouteDisplayConfig -> PrestoDOM (Effect Unit) w
routeNumberView config = 
  if config.useHtmlFormatting 
    then textView $ [
          textFromHtml $ "<span><strong>" <> config.routeNumber <> "</strong></span>"
        , color Colors.black900
        , height WRAP_CONTENT
        , width WRAP_CONTENT
        , maxLines 2
        , ellipsize true
        ] <> (FontStyle.getFontStyle config.routeFontSize LanguageStyle)
    else textView $ [
          text config.routeNumber
        , color config.routeNumberColor
        ] <> (FontStyle.getFontStyle config.routeFontSize LanguageStyle)

dotSeparator :: forall w. RouteDisplayConfig -> PrestoDOM (Effect Unit) w
dotSeparator config = 
  linearLayout [
    color Colors.black600
  , margin $ Margin 8 0 8 0
  , background Colors.black600
  , gravity CENTER
  , width $ V 6
  , height $ V 6
  , cornerRadius 32.0 
  , visibility $ boolToVisibility config.showDot
  ] []

routeDetailsView :: forall w. RouteDisplayConfig -> PrestoDOM (Effect Unit) w
routeDetailsView config =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
    , weight 1.0
    , visibility $ boolToVisibility config.showRouteDetails
    ][
      locationText config.sourceName config.fontSize
    , arrowText config.fontSize
    , locationText config.destinationName config.fontSize
    ]

locationText :: forall w. String -> Style -> PrestoDOM (Effect Unit) w
locationText content fontSize =
  textView $ [
      text content
    , color Colors.black700
    , ellipsize true
    , singleLine true
    , weight 1.0
    ] <> (FontStyle.getFontStyle fontSize LanguageStyle)

arrowText :: forall w. Style -> PrestoDOM (Effect Unit) w
arrowText fontSize =
  textView $ [
      text " → "
    , color Colors.black700
    , ellipsize true
    , singleLine true
    , weight 1.0
    ] <> (FontStyle.getFontStyle fontSize LanguageStyle)

chevronView :: forall w. RouteDisplayConfig -> PrestoDOM (Effect Unit) w
chevronView config =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , gravity CENTER_VERTICAL
    , visibility $ boolToVisibility config.showChevron
    ][
      imageView
        [ imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_chevron_down"
        , width $ V 8
        , height $ V 12
        , gravity CENTER
        , layoutGravity "left"
        , margin $ MarginHorizontal 8 0
        ]
    ]