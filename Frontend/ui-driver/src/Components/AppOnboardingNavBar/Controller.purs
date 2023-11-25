module Components.AppOnboardingNavBar.Controller where

import MerchantConfig.Types (AppConfig)
import PrestoDOM(Length(..), Visibility(..), Margin(..))
import Components.GenericHeader as GenericHeader
import MerchantConfig.DefaultConfig as DC
import Font.Style (Style(..))
import Styles.Colors as Color
import Font.Style as Style

data Action = Logout 
            | PrefixImgOnClick 
            | GenericHeaderAC GenericHeader.Action

type Config = {
  prefixImageConfig :: ImageConfig,
  genericHeaderConfig :: GenericHeader.Config,
  headerTextConfig :: TextConfig,
  appConfig :: AppConfig
}

type ImageConfig = {
  height :: Length,
  width :: Length,
  visibility :: Visibility,
  image :: String
}

type TextConfig = {
  text :: String,
  margin :: Margin,
  color :: String,
  fontStyle :: Style
}

config :: Config
config = {
  prefixImageConfig : {
    height : V 25,
    width : V 25,
    visibility : VISIBLE,
    image : "ny_ic_chevron_left_white"
  },
  genericHeaderConfig : GenericHeader.config,
  headerTextConfig : {
    text : "",
    margin : Margin 5 5 0 22 ,
    color : Color.white900,
    fontStyle : Style.Heading1
  },
  appConfig : DC.config
}