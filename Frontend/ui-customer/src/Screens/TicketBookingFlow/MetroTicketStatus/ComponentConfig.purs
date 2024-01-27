module Screens.TicketBookingFlow.MetroTicketStatus.ComponentConfig where

import Prelude

import Common.Styles.Colors as Color
import Components.PrimaryButton as PrimaryButton
import Data.Maybe (Maybe(..), fromMaybe)
import Font.Style (Style(..))
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..), visibility)
import Accessor (_name)
import Data.Lens ((^.))
import Screens.Types as ST
import Data.Array as DA
import JBridge as JB


refreshStatusButtonConfig :: ST.MetroTicketStatusScreenState -> PrimaryButton.Config
refreshStatusButtonConfig state = PrimaryButton.config
    { textConfig 
      { text = "Refresh Status"
      , textStyle = Tags
      , weight = Just 1.0
      , gravity = CENTER
      , color = Color.black800
      }
      , height = WRAP_CONTENT
      , gravity = CENTER
      , cornerRadius = 32.0
      , width = MATCH_PARENT
      , padding =  Padding 0 13 0 13
      , margin = Margin 16 16 16 0
      , isPrefixImage = true
      , background = Color.white900
      , prefixImageConfig
        { imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_history_unfilled"
        , height = V 15
        , width = V 15
        , margin = MarginRight 5
        }
      , id = "RefershPaymentStatusButton"
    }

tryAgainBtnConfig :: String -> Visibility -> PrimaryButton.Config
tryAgainBtnConfig text visibility' = 
  PrimaryButton.config { 
  textConfig
      { text = text
      , color = Color.yellow900
      }
    , cornerRadius = 8.0
    , background = Color.black900 
    , visibility =  visibility'
    , id = "ViewTicketsButton"
    , margin = (Margin 16 16 16 16)
    }
