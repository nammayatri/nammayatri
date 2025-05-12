module Components.SelectableItem where

import Prelude (Unit, const, ($), (<>), (==))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, 
  background, color, cornerRadius, ellipsize, gravity, height, imageView, imageWithFallback, 
  layoutGravity, linearLayout, margin, maxLines, onClick, orientation, padding, 
  rippleColor, singleLine, stroke, text, textFromHtml, textView, visibility, weight, width)
import Common.Types.App (LazyCheck(LanguageStyle))
import Helpers.Utils as HU
import Font.Style as FontStyle
import Common.Styles.Colors as Colors
import Mobility.Prelude (boolToVisibility)
import Effect (Effect)
import Font.Style (Style)
import Styles.Types (Color, FontStyle(..))
import Data.Maybe (Maybe(..), fromMaybe)
import PrestoDOM.Types.DomAttributes (Visibility(..))

data Action = NoAction | Click | Select Int String

type ImageConfig = {
  imageUrl :: String,
  height :: Length,
  width :: Length,
  margin :: Margin,
  visibility :: Boolean,
  padding :: Padding
}

type SelectableItemConfig = {
    primaryText :: String                -- Main text (route number, conductor name)
  , primaryTextColor :: Color            -- Color for primary text
  , secondaryText :: Maybe String        -- Optional secondary text (source)
  , destinationText :: Maybe String      -- Optional destination text
  , onClick :: Action                    -- Action to perform on click
  , showChevron :: Boolean               -- Whether to show the chevron icon
  , isSelectable :: Boolean              -- Whether the item is selectable
  , isSelected :: Boolean                -- Whether the item is selected
  , cornerRadius :: Number               -- Corner radius of the item
  , margin :: Margin                     -- Margin around the item
  , padding :: Padding                   -- Padding inside the item
  , useHtmlFormatting :: Boolean         -- Whether to use HTML formatting
  , fontSize :: Style                    -- Font size for secondary text
  , primaryFontSize :: Style             -- Font size for primary text
  , leadingIcon :: Maybe ImageConfig     -- Optional icon at the start
  , trailingIcon :: Maybe ImageConfig    -- Optional icon at the end
  , backgroundColor :: Color             -- Background color
  , selectedBackgroundColor :: Color     -- Background color when selected
  , borderColor :: Color                 -- Border color
  , selectedBorderColor :: Color         -- Border color when selected
  , showDot :: Boolean
}

defaultConfig :: SelectableItemConfig
defaultConfig = {
    primaryText: ""
  , primaryTextColor: Colors.black900
  , secondaryText: Nothing
  , destinationText: Nothing
  , onClick: NoAction
  , showChevron: false
  , isSelectable: true
  , isSelected: false
  , cornerRadius: 8.0
  , margin: Margin 0 0 0 0
  , padding: Padding 16 16 16 16
  , useHtmlFormatting: false
  , fontSize: FontStyle.Body16
  , primaryFontSize: FontStyle.SubHeading3
  , leadingIcon: Nothing
  , trailingIcon: Nothing
  , backgroundColor: Colors.white900
  , selectedBackgroundColor: Colors.blue600
  , borderColor: Colors.grey900
  , selectedBorderColor: Colors.blue900
  , showDot : false
}

-- Helper function to create a route config from the default config
routeConfig :: SelectableItemConfig
routeConfig = defaultConfig {
  showChevron = true,
  useHtmlFormatting = false
}

-- Helper function to create a conductor config from the default config
conductorConfig :: SelectableItemConfig
conductorConfig = defaultConfig {
  showChevron = false,
  secondaryText = Nothing,
  destinationText = Nothing
}

view :: forall w. (Action -> Effect Unit) -> SelectableItemConfig -> PrestoDOM (Effect Unit) w
view push config = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , padding config.padding
    , cornerRadius config.cornerRadius
    , margin config.margin
    , stroke $ "1," <> (if config.isSelected then config.selectedBorderColor else config.borderColor)
    , gravity CENTER_VERTICAL
    , background (if config.isSelected then config.selectedBackgroundColor else config.backgroundColor)
    , onClick push $ const config.onClick
    , rippleColor Colors.rippleShade
    ][
      leadingIconView config
    , primaryTextView config
    , dotView config
    , routeDetailsView config
    , chevronView config
    , trailingIconView config
    ]

leadingIconView :: forall w. SelectableItemConfig -> PrestoDOM (Effect Unit) w
leadingIconView config = 
  case config.leadingIcon of
    Just icon ->
      imageView
        [ imageWithFallback icon.imageUrl
        , width icon.width
        , height icon.height
        , margin icon.margin
        , padding icon.padding
        , visibility $ boolToVisibility icon.visibility
        ]
    Nothing -> textView [visibility GONE]

primaryTextView :: forall w. SelectableItemConfig -> PrestoDOM (Effect Unit) w
primaryTextView config = 
  if config.useHtmlFormatting 
    then textView $ [
          textFromHtml $ "<b>" <> config.primaryText <> "</b>"
        , color config.primaryTextColor
        , height WRAP_CONTENT
        , width WRAP_CONTENT
        , maxLines 1
        , ellipsize true
        , gravity LEFT
        ] <> (FontStyle.getFontStyle config.primaryFontSize LanguageStyle)
    else textView $ [
          text config.primaryText
        , color config.primaryTextColor
        , height WRAP_CONTENT
        , width WRAP_CONTENT
        , maxLines 1
        , ellipsize true
        , gravity LEFT
        ] <> (FontStyle.getFontStyle config.primaryFontSize LanguageStyle)

routeDetailsView :: forall w. SelectableItemConfig -> PrestoDOM (Effect Unit) w
routeDetailsView config =
  case config.secondaryText of
    Just source ->
      linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        , weight 1.0
        ][
          locationText source config.fontSize
        , case config.destinationText of
            Just dest -> 
              linearLayout [
                height WRAP_CONTENT
              , width WRAP_CONTENT
              , orientation HORIZONTAL
              , gravity CENTER_VERTICAL
              ] [
                arrowText config.fontSize
              , locationText dest config.fontSize
              ]
            Nothing -> textView [visibility GONE]
        ]
    Nothing -> textView [visibility GONE]

locationText :: forall w. String -> Style -> PrestoDOM (Effect Unit) w
locationText content fontSize =
  textView $ [
      text content
    , color Colors.black700
    , ellipsize true
    , singleLine true
    , gravity CENTER_VERTICAL
    ] <> (FontStyle.getFontStyle fontSize LanguageStyle)

arrowText :: forall w. Style -> PrestoDOM (Effect Unit) w
arrowText fontSize =
  textView $ [
      text " → "
    , color Colors.black700
    , ellipsize true
    , singleLine true
    , gravity CENTER_VERTICAL
    , margin $ MarginHorizontal 4 4
    ] <> (FontStyle.getFontStyle fontSize LanguageStyle)

dotView :: forall w. SelectableItemConfig -> PrestoDOM (Effect Unit) w
dotView config = 
  linearLayout [
    margin $ MarginHorizontal 8 8
  , background Colors.grey900
  , gravity CENTER
  , width $ V 4
  , height $ V 4
  , cornerRadius 2.0 
  , visibility $ boolToVisibility config.showDot
  ] []

chevronView :: forall w. SelectableItemConfig -> PrestoDOM (Effect Unit) w
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
        , layoutGravity "right"
        , margin $ MarginRight 0
        ]
    ]

trailingIconView :: forall w. SelectableItemConfig -> PrestoDOM (Effect Unit) w
trailingIconView config = 
  case config.trailingIcon of
    Just icon ->
      imageView
        [ imageWithFallback icon.imageUrl
        , width icon.width
        , height icon.height
        , margin icon.margin
        , padding icon.padding
        , visibility $ boolToVisibility icon.visibility
        ]
    Nothing -> textView [visibility GONE] 