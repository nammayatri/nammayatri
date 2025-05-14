module Components.SelectableItems where

import Prelude
import Prelude (Unit, const, ($), (==), (<>), (<<<), map, bind, pure, unit, (-))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), 
  clickable, fontStyle, imageUrl, imageView, textFromHtml, textSize, weight, afterRender, editText, alignParentBottom, imageWithFallback, stroke, layoutGravity,
  background, gravity, height, linearLayout, margin, onClick, orientation, padding, 
  width, scrollView, visibility, cornerRadius, text, textView, color)
import Common.Types.App (LazyCheck(LanguageStyle))
import Effect (Effect)
import Data.Array (mapWithIndex, length)
import Data.Maybe (Maybe(..), fromMaybe)
import Styles.Colors as Colors
import Mobility.Prelude (boolToVisibility)
import Components.SelectableItem as SelectableItem
import Font.Style as FontStyle
import Styles.Types (FontStyle(..))

data Action = 
  NoAction 
  | ItemSelected Int String 
  | ItemClicked

type ContainerStyleConfig = {
  margin :: Margin,
  padding :: Padding,
  background :: String,
  maxHeight :: Maybe Length,
  fullScreen :: Boolean
}

type SelectableItemsConfig a = {
  items :: Array a,                          -- Array of data items 
  itemRenderer :: a -> Int -> SelectableItem.SelectableItemConfig,  -- Function to convert data to config
  selectedIndex :: Maybe Int,                -- Currently selected index
  -- onItemSelect :: Int -> String -> Effect Unit, -- Selection handler
  containerStyle :: ContainerStyleConfig,    -- Styling for the container
  showSearch :: Boolean,                     -- Whether to show search box
  searchPlaceholder :: String,              -- Placeholder for search box
  visibility :: Visibility
}

defaultConfig :: forall a. SelectableItemsConfig a
defaultConfig = {
  items: [],
  itemRenderer: \_ _ -> SelectableItem.defaultConfig,
  selectedIndex: Nothing,
  -- onItemSelect: \_ _ -> pure unit,
  containerStyle: {
    margin: Margin 0 0 0 0,
    padding: Padding 0 0 0 0,
    background: Colors.white900,
    maxHeight: Nothing,
    fullScreen: false
  },
  showSearch: false,
  searchPlaceholder: "Search",
  visibility : GONE
}

view :: forall w a. (Action -> Effect Unit) -> SelectableItemsConfig a -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout
    [ height (fromMaybe MATCH_PARENT config.containerStyle.maxHeight)
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin config.containerStyle.margin
    , padding config.containerStyle.padding
    , background config.containerStyle.background
    ] [
      searchBoxView config,
      itemsContainer push config
    ]

searchBoxView :: forall w a. SelectableItemsConfig a -> PrestoDOM (Effect Unit) w
searchBoxView config =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , visibility $ boolToVisibility config.showSearch
    , margin $ Margin 16 8 16 8
    ] [
      linearLayout
        [ height $ V 48
        , width MATCH_PARENT
        , background Colors.grey700
        , cornerRadius 8.0
        , padding $ Padding 12 0 12 0
        ] [
          -- You could enhance this with an actual search icon and functionality
          textView $ [
            text config.searchPlaceholder,
            color Colors.grey900,
            gravity CENTER_VERTICAL,
            height MATCH_PARENT,
            width MATCH_PARENT
          ] <> (FontStyle.body1 LanguageStyle)
        ]
    ]

itemsContainer :: forall w a. (Action -> Effect Unit) -> SelectableItemsConfig a -> PrestoDOM (Effect Unit) w
itemsContainer push config =
  scrollView
    [ height MATCH_PARENT
    , width MATCH_PARENT
    ] [
      linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , gravity CENTER_HORIZONTAL
        , margin $ MarginTop 8
        ] $ 
        mapWithIndex 
          (\index item -> 
            let 
              itemConfig = config.itemRenderer item index
              isSelected = Just index == config.selectedIndex
              updatedConfig = itemConfig { 
                isSelected = isSelected,
                onClick = 
                  case itemConfig.onClick of
                    SelectableItem.Select idx val -> SelectableItem.Select idx val
                    _ -> SelectableItem.Select index (itemConfig.primaryText)
              }
            in
              linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , margin $ if index == (length config.items - 1) 
                           then Margin 16 0 16 16
                           else Margin 16 0 16 8
                ] [
                  SelectableItem.view 
                    (push <<< (\action -> case action of
                      SelectableItem.Select idx val -> ItemSelected idx val
                      SelectableItem.Click -> ItemClicked
                      _ -> NoAction)) 
                    updatedConfig
                ]
          ) 
          config.items
    ] 