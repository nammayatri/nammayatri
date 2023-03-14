module Screens.CustomerUtils.TripDetailsScreen.ComponentConfig where

import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Language.Types (STR(..))
import Language.Strings (getString)
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..))
import Prelude (Unit, const, map, ($), (&&), (/=), (<<<), (<=), (<>), (==), (||))
import Screens.Types as ST
import Font.Size as FontSize
import Font.Style as FontStyle
import Components.GenericHeader as GenericHeader
import Components.SourceToDestination as SourceToDestination
import Styles.Colors as Color
import Common.Types.App

genericHeaderConfig :: ST.TripDetailsScreenState -> GenericHeader.Config 
genericHeaderConfig state= let 
  config = GenericHeader.config
  genericHeaderConfig' = config 
    {
      height = WRAP_CONTENT
     , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/common/ny_ic_chevron_left.png"
      , margin = (Margin 12 12 12 12)
      , visibility = if state.props.issueReported then GONE else VISIBLE
      }
    , textConfig {
        text = if state.props.issueReported then "" else (getString RIDE_DETAILS)
      , textSize = FontSize.a_18
      , color = Color.darkDescriptionText
      , fontStyle = FontStyle.bold LanguageStyle
      }
    , suffixImageConfig {
        visibility = GONE
      }
    , padding = (Padding 0 5 0 5)
    }
  in genericHeaderConfig'
  
confirmLostAndFoundConfig :: ST.TripDetailsScreenState ->  PopUpModal.Config 
confirmLostAndFoundConfig state = let 
    config' = PopUpModal.config 
    popUpConfig' = config' {
      primaryText { text = (getString LOST_SOMETHING)},
      secondaryText {
        text = (getString TRY_CONNECTING_WITH_THE_DRIVER)
      , margin = (Margin 0 4 0 20)},
      option1 {text = (getString CANCEL_)},
      option2 {text = (getString CALL_DRIVER),margin = (Margin 12 0 0 0)}
    }
    in popUpConfig'

sourceToDestinationConfig :: ST.TripDetailsScreenState -> SourceToDestination.Config
sourceToDestinationConfig state = let 
  config = SourceToDestination.config
  sourceToDestinationConfig' = config
    {
      margin = (Margin 0 0 0 0)
    , sourceMargin = (Margin 0 0 0 25)
    , lineMargin = (Margin 4 4 0 0)
    , sourceImageConfig {
        imageUrl = "ny_ic_green_circle,https://assets.juspay.in/nammayatri/images/common/ny_ic_green_circle.png"
      , margin = (MarginTop 3)
      }
    , sourceTextConfig {
        text = state.data.source
      , textSize = FontSize.a_12
      , padding = (Padding 2 0 2 2)
      , margin = (Margin 12 0 15 0)
      , fontStyle = FontStyle.regular LanguageStyle
      , color = Color.greyDavy
      , ellipsize = false
      }
    , destinationImageConfig {
        imageUrl = "ny_ic_red_circle,https://assets.juspay.in/nammayatri/images/common/ny_ic_red_circle.png"
      , margin = (MarginTop 3)
      }
    , destinationBackground = Color.blue600
    , destinationTextConfig {
        text = state.data.destination
      , textSize = FontSize.a_12
      , padding = (Padding 2 0 2 2)
      , margin = (Margin 12 0 15 0)
      , fontStyle = FontStyle.regular LanguageStyle
      , color = Color.greyDavy
      , ellipsize = false
      }
    }
  in sourceToDestinationConfig'

primaryButtonConfig :: ST.TripDetailsScreenState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig { 
          text = if state.props.issueReported then (getString GO_HOME_) else (getString SUBMIT)
        , color = if (state.props.activateSubmit) || (state.props.issueReported ) then Color.yellow900 else "#FEEBB9"
        , fontStyle = FontStyle.bold LanguageStyle
        , textSize = FontSize.a_16
        }
      , height = V 48
      , width = MATCH_PARENT
      , background = if (state.props.activateSubmit || state.props.issueReported)  then Color.black900 else "#B9BABE"
      , alpha = if (state.props.activateSubmit || state.props.issueReported)  then 1.0 else 0.5 
      , isClickable = (state.props.activateSubmit || state.props.issueReported) 
      , margin = (Margin 16 0 16 16 ) 
      }
  in primaryButtonConfig'
