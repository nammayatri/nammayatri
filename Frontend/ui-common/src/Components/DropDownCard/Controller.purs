module Components.DropDownCard.Controller where
 
import Prelude
import Font.Style as FontStyle
import Styles.Colors as Color 
import PrestoDOM
import Effect (Effect)
import Styles.Types (Color)



instance showAction :: Show Action where
  show (NoAction) = "NoAction"
  show (OnClick _) = "OnClick"


data Action = NoAction 
              | OnClick Config


type Config = 
  { isOpen :: Boolean
  , title :: String
  , layout :: forall w . PrestoDOM (Effect Unit) w
  , openArrowImage :: String
  , closeArrowImage :: String 
  , id :: String
  , titleBackground :: Color
  , cardMargin :: Margin
  , cardPadding :: Padding
  , headingPadding :: Padding
  , imageHeight :: Length
  , imageWidth :: Length
  , headingCornerRadius :: Number
  }



-- dummyConfig :: Config
-- dummyConfig = {
--   isOpen : false,
--   title : "Dummy Card",
--   layout : linearLayout 
--             [ height WRAP_CONTENT
--             , width WRAP_CONTENT
--             ][]
--   , openArrowImage : "ny_ic_chevron_down"
--   , closeArrowImage : "ny_ic_chevron_up"
-- }