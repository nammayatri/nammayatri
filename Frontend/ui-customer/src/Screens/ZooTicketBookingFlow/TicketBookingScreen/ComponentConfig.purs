module Screens.TicketBookingScreen.ComponentConfig where

import Prelude
import Components.GenericHeader as GenericHeader
import Screens.Types as ST
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..), visibility)
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Common.Styles.Colors as Color

genericHeaderConfig :: ST.TicketBookingScreenState -> GenericHeader.Config
genericHeaderConfig state = let
  config = GenericHeader.config
  genericHeaderConfig' = config 
    {
      height = WRAP_CONTENT
    , prefixImageConfig {
       visibility = VISIBLE
      , imageUrl = fetchImage FF_ASSET "ny_ic_chevron_left"
      , height = V 25
      , width = V 25
      , margin = Margin 16 16 16 16
      } 
    , padding = PaddingVertical 5 5
    , textConfig {
        text = case state.props.currentStage of
                  ST.DescriptionStage -> "Zoological Garden, Alipore"
                  ST.ChooseTicketStage -> "Choose Tickets"
                  ST.ViewTicketStage -> "Zoological Garden, Alipore"
                  _ -> ""
      , color = Color.darkCharcoal
      }
    , suffixImageConfig {
        visibility = GONE
      }
    }
  in genericHeaderConfig'