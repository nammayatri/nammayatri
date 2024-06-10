module Screens.WelcomeScreen.ComponentConfig where

import Prelude
import Common.Styles.Colors as Color
import Common.Types.App as Common
import Components.PrimaryButton as PrimaryButton
import Language.Strings (getString)
import Language.Types (STR(..))
import Screens.Types (WelcomeScreenState)
import ConfigProvider
import Screens.WelcomeScreen.ScreenData
import Engineering.Helpers.Commons as EHC
import PrestoDOM (Margin(..))
import MerchantConfig.Utils as MCU

carouselData :: WelcomeScreenState -> Common.CarouselModal
carouselData state =
  let config = getAppConfig appConfig
  in
  { gravity: "CENTER"
  , carouselData:
      map
        ( \item ->
            { imageConfig: { image: item.image, height: item.imageHeight, width: 200, bgColor: config.welcomeScreen.background, cornerRadius: 8.0 , isUrl : true }
            , backgroundColor: config.welcomeScreen.background
            , youtubeConfig: dummyYoutubeData
            , gravity: item.gravity
            , contentType: "IMAGE"
            , titleConfig:
                { text: item.title
                , textSize: 22
                , textColor: Color.black800
                , gravity: "CENTER"
                , margin: { top: 10, bottom: 0, right: 20, left: 20 }
                }
            , descriptionConfig:
                { text: item.description
                , textSize: 16
                , textColor: Color.black700
                , gravity: "CENTER"
                , margin: { top: 6, bottom: 0, right: 20, left: 20 }
                }
            }
        ) (carouselTextData state)
        
  }
carouselTextData :: WelcomeScreenState -> Array CarouselData
carouselTextData state = 
  let isBridge = MCU.getMerchant Common.FunctionCall == MCU.BRIDGE
  in [ { image: if isBridge then "ny_ic_welcome_screen_1" else "https://assets.moving.tech/beckn/common/driver/images/ny_img_welcome_first.png", title: getString $ DIRECT_PAYMENT_NO_COMMISSIONS "DIRECT_PAYMENT_NO_COMMISSIONS", description: getString $ CUSTOMER_PAYS_DIRECTLY "CUSTOMER_PAYS_DIRECTLY", gravity: 17, imageHeight: 260 }
        , { image:  if isBridge then "ny_ic_welcome_screen_2" else  "https://assets.moving.tech/beckn/common/driver/images/ny_img_welcome_second.png" , title: getString $ HUNDRED_PERCENT_FARE_GOES_TO_YOU "HUNDRED_PERCENT_FARE_GOES_TO_YOU", description: getString $ FARE_SHOWN_IS_FARE_YOU_GET "FARE_SHOWN_IS_FARE_YOU_GET", gravity: 17, imageHeight: 260 }
        , { image:  if isBridge then "ny_ic_welcome_screen_3" else "https://assets.moving.tech/beckn/common/driver/images/ny_img_welcome_third.png" , title: getString $ BE_A_PART_OF_OPEN_MOBILITY_REVOLUTION "BE_A_PART_OF_OPEN_MOBILITY_REVOLUTION", description: getString $ OUR_DATA_AND_PRODUCT_ARE_TRANSPARENT "OUR_DATA_AND_PRODUCT_ARE_TRANSPARENT", gravity: 17, imageHeight: 260 }
        ]

primaryButtonConfig :: WelcomeScreenState -> PrimaryButton.Config
primaryButtonConfig _state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig { text = getString GET_STARTED }
      , id = "PrimaryButtonWelcomeScreen"
      , enableRipple = true
      , margin = Margin 10 24 10 0
      }
  in primaryButtonConfig'

dummyYoutubeData :: Common.YoutubeData
dummyYoutubeData =
  { videoTitle: ""
  , setVideoTitle: false
  , showMenuButton: false
  , showDuration: true
  , showSeekBar: true
  , videoId: ""
  , videoType: ""
  , videoHeight: 0
  , showFullScreen : false
  , hideFullScreenButton : false
  }
