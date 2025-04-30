module Screens.WelcomeScreen.ComponentConfig where

import Prelude
import Common.Styles.Colors as Color
import Common.Types.App as Common
import Components.PrimaryButton as PrimaryButton
import Language.Strings (getString)
import Language.Types (STR(..))
import Screens.Types (WelcomeScreenState)
import Helpers.Utils as HU

carouselData :: WelcomeScreenState -> Common.CarouselModal
carouselData state =
  { gravity: "CENTER"
  , carouselData:
      map
        ( \item ->
            { imageConfig: { image: item.image, height: item.imageHeight, width: 200, bgColor: state.data.config.themeColors.welcomeScreenBackground, cornerRadius: 8.0 , isUrl : true }
            , backgroundColor: state.data.config.themeColors.welcomeScreenBackground
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
        )
        [ { image: (HU.getAssetLink Common.FunctionCall) <> "ny_img_welcome_first" <> ".png", title: getString DIRECT_PAYMENT_NO_COMMISSIONS, description: getString CUSTOMER_PAYS_DIRECTLY, gravity: 17, imageHeight: 260 }
        , { image: (HU.getAssetLink Common.FunctionCall) <> "ny_img_welcome_second" <> ".png", title: getString HUNDRED_PERCENT_FARE_GOES_TO_YOU, description: getString FARE_SHOWN_IS_FARE_YOU_GET, gravity: 17, imageHeight: 260 }
        , { image: (HU.getAssetLink Common.FunctionCall) <> "ny_img_welcome_third" <> ".png", title: getString BE_A_PART_OF_OPEN_MOBILITY_REVOLUTION, description: getString OUR_DATA_AND_PRODUCT_ARE_TRANSPARENT, gravity: 17, imageHeight: 260 }
        ]
  }

primaryButtonConfig :: WelcomeScreenState -> PrimaryButton.Config
primaryButtonConfig _state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig { text = getString GET_STARTED }
      , id = "PrimaryButtonWelcomeScreen"
      , enableRipple = true
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
