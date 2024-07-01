module Components.DropDownCard.Controller
  ( Action(..)
  , Config
  )
  where

import Prelude
import Font.Style as FontStyle
import Styles.Colors as Color 
import PrestoDOM
import Effect (Effect)




data Action = NoAction 
              | OnClick


type Config = 
  { isOpen :: Boolean
  , title :: String
  , layout :: forall w . PrestoDOM (Effect Unit) w
  , openArrowImage :: String
  , closeArrowImage :: String 
  }



-- dummyConfig :: Config
-- dummyConfig = {
--   isOpen : false,
--   title : "Dummy Card",
--   layout : linearLayout 
--             [ height WRAP_CONTENT
--             , width WRAP_CONTENT
--             ][]
-- }