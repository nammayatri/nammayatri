module Components.SelectRouteButton where

import Prelude (Unit, const, ($), (<>))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM)
import Common.Types.App (LazyCheck(..))
import Helpers.Utils as HU
import Font.Style as FontStyle
import Styles.Colors as Colors
import Mobility.Prelude (boolToVisibility)
import Effect (Effect)
import Font.Style (Style)
import Styles.Types (Color)
import Data.Maybe (Maybe(..))
import Components.SelectableItem as SelectableItem

data Action = NoAction | Click | Select Int String

-- Original config maintained for backward compatibility
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
  , routeFontSize: FontStyle.SubHeading3
}

-- Wrapper that maintains the old API but uses the new component
view :: forall w. (Action -> Effect Unit) -> RouteDisplayConfig -> PrestoDOM (Effect Unit) w
view push config = 
  SelectableItem.view 
    (\action -> case action of
      SelectableItem.Select idx val -> push (Select idx val)
      SelectableItem.Click -> push Click
      _ -> push NoAction)
    (convertConfig config)

-- Convert the old config to the new SelectableItem config
convertConfig :: RouteDisplayConfig -> SelectableItem.SelectableItemConfig
convertConfig config = 
  { primaryText: config.routeNumber
  , primaryTextColor: config.routeNumberColor
  , secondaryText: if config.showRouteDetails then Just config.sourceName else Nothing
  , destinationText: if config.showRouteDetails then Just config.destinationName else Nothing
  , onClick: 
      case config.onClick of
        Select idx val -> SelectableItem.Select idx val
        Click -> SelectableItem.Click
        _ -> SelectableItem.NoAction
  , showChevron: config.showChevron
  , isSelectable: config.isSelectable
  , isSelected: false
  , cornerRadius: config.cornerRadius
  , margin: config.margin
  , padding: config.padding
  , useHtmlFormatting: config.useHtmlFormatting
  , fontSize: config.fontSize
  , primaryFontSize: config.routeFontSize
  , leadingIcon: Nothing
  , trailingIcon: Nothing
  , backgroundColor: Colors.white900
  , selectedBackgroundColor: Colors.blue600 
  , borderColor: Colors.grey900
  , selectedBorderColor: Colors.blue900
  , showDot: config.showDot
  }