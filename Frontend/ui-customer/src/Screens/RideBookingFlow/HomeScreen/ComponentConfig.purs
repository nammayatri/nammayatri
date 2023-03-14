module Screens.RideBookingFlow.HomeScreen.Config where

import Prelude
import Screens.Types as ST
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.SourceToDestination as SourceToDestination
import Components.CancelRide as CancelRidePopUpConfig
import Components.FareBreakUp as FareBreakUp
import Components.ErrorModal as ErrorModal
import Components.RateCard as RateCard
import Components.DriverInfoCard as DriverInfoCard
import Components.EmergencyHelp as EmergencyHelp
import Components.SearchLocationModel as SearchLocationModel
import Components.QuoteListModel as QuoteListModel
import Components.RatingCard as RatingCard
import PrestoDOM.Types.DomAttributes (Corners(..))
import Data.String as DS
import Animation.Config as AnimConfig
import Data.Array as DA
import Data.Maybe (Maybe(..), fromMaybe)
import Language.Strings
import Language.Types (STR(..))
import Helpers.Utils as HU
import Engineering.Helpers.Commons as EHC
import JBridge as JB
import Font.Size as FontSize
import Font.Style as FontStyle
import Styles.Colors as Color
import Common.Types.App
import PrestoDOM

shareAppConfig :: ST.HomeScreenState -> PopUpModal.Config
shareAppConfig state = let
  config' = PopUpModal.config
  popUpConfig' = config'{
      gravity = CENTER,
      margin = (MarginHorizontal 24 24),
      buttonLayoutMargin = (Margin 16 0 16 20),
      primaryText {
        text = getString(YOUR_RIDE_HAS_STARTED) 
      , margin = (Margin 16 0 16 0)},
      secondaryText { 
        text = getString(ENJOY_RIDING_WITH_US)
      , margin = Margin 0 12 0 24 
      , fontSize = FontSize.a_14
      , color = Color.black700},
      option1 {
        text = getString(MAYBE_LATER) 
      , fontSize = FontSize.a_16
      , width = V $ (((EHC.screenWidth unit)-92)/2) 
      , background = Color.white900
      , strokeColor = Color.black500
      , color = Color.black700
      , fontStyle = FontStyle.semiBold LanguageStyle
      },
      option2 {
        text = getString(SHARE_APP) 
      , fontSize = FontSize.a_16
      , width = V $ (((EHC.screenWidth unit)-92)/2)
      , color = Color.yellow900
      , strokeColor = Color.black900
      , background = Color.black900
      , margin = MarginLeft 12
      ,fontStyle = FontStyle.semiBold LanguageStyle
      },
      cornerRadius = (Corners 15.0 true true true true),
      coverImageConfig {
        imageUrl = "ny_ic_share_app,https://assets.juspay.in/nammayatri/images/user/ny_ic_share_app.png"
      , visibility = VISIBLE
      , margin = Margin 16 20 16 24
      , width = MATCH_PARENT
      , height = V 200
      }
  }
  in popUpConfig'


skipButtonConfig :: ST.HomeScreenState -> PrimaryButton.Config
skipButtonConfig state =
  let
    config = PrimaryButton.config
    skipButtonConfig' =
      config
        { textConfig
          { text = (getString SKIP)
          , color = Color.black700
          , fontStyle = FontStyle.bold LanguageStyle
          , textSize = FontSize.a_16
          }
        , width = V (EHC.screenWidth unit / 4)
        , background = Color.white900
        , stroke = ("1," <> Color.black500)
        , margin = (Margin 0 0 0 0)
        , id = "SkipRatingButton"
        , enableLoader = (JB.getBtnLoader "SkipRatingButton")
        }
  in
    skipButtonConfig'

sourceToDestinationConfig :: ST.HomeScreenState -> SourceToDestination.Config
sourceToDestinationConfig state =
  let
    config = SourceToDestination.config
    sourceToDestinationConfig' =
      config
        { margin = (Margin 0 0 0 0)
        , width = V (EHC.screenWidth unit - 70)
        , sourceMargin = (Margin 0 0 0 14)
        , lineMargin = (Margin 20 10 0 0)
        , sourceImageConfig
          { imageUrl = "ny_ic_source_dot,https://assets.juspay.in/nammayatri/images/common/ny_ic_source_dot.png"
          , height = V 13
          , width = V 13
          , margin = (Margin 14 2 0 0)
          }
        , rideStartedAtConfig
          { text = state.data.startedAt
          , visibility = VISIBLE
          , padding = (Padding 1 1 1 1)
          , margin = (Margin 12 2 0 0)
          }
        , sourceTextConfig
          { text = state.data.driverInfoCardState.source
          , textSize = FontSize.a_14
          , padding = (Padding 0 0 0 0)
          , margin = (Margin 11 0 15 0)
          , fontStyle = FontStyle.medium LanguageStyle
          , color = Color.black800
          , ellipsize = true
          , maxLines = 1
          }
        , destinationImageConfig
          { imageUrl = "ny_ic_loc_red,https://assets.juspay.in/nammayatri/images/common/ny_ic_loc_red.png"
          , height = V 17
          , width = V 14
          , margin = (Margin 13 3 0 0)
          }
        , destinationTextConfig
          { text = state.data.driverInfoCardState.destination
          , textSize = FontSize.a_14
          , padding = (Padding 2 0 2 2)
          , margin = (Margin 10 0 15 0)
          , maxLines = 1
          , color = Color.black800
          , fontStyle = FontStyle.medium LanguageStyle
          , ellipsize = true
          }
        , rideEndedAtConfig
          { text = state.data.endedAt
          , visibility = VISIBLE
          , padding = (Padding 1 1 1 1)
          , margin = (Margin 13 2 0 0)
          }
        }
  in
    sourceToDestinationConfig'

fareBreakUpConfig :: ST.HomeScreenState -> FareBreakUp.Config
fareBreakUpConfig state =
  let
    config = FareBreakUp.config
    fareBreakUpConfig' =
      config
        { fareDetails = []
        , headingText = (getString VIEW_BREAKDOWN)
        , totalAmount =
          { text: ""
          , textSize: FontSize.a_16
          , fontStyle: FontStyle.semiBold LanguageStyle
          , color: Color.black800
          , margin: (Margin 0 0 0 20)
          , visibility: GONE
          , priceDetails:
              { text: 0
              , textSize: FontSize.a_16
              , fontStyle: FontStyle.semiBold LanguageStyle
              , offeredFare: state.data.driverInfoCardState.price
              , distanceDifference: state.data.previousRideRatingState.distanceDifference
              }
          }
        , rideDetails =
          { destination: state.data.driverInfoCardState.destination
          , destinationTitle: (fromMaybe "" ((DS.split (DS.Pattern ",") (state.data.driverInfoCardState.destination)) DA.!! 0))
          , source: state.data.driverInfoCardState.source
          , sourceTitle: (fromMaybe "" ((DS.split (DS.Pattern ",") (state.data.driverInfoCardState.source)) DA.!! 0))
          , rideStartTime: state.data.startedAt
          , rideStartDate: ((fromMaybe "" ((DS.split (DS.Pattern ",") (HU.convertUTCtoISC (state.data.startedAtUTC) "llll")) DA.!! 0)) <> ", " <> (HU.convertUTCtoISC (state.data.startedAtUTC) "Do MMM"))
          , estimatedDistance: state.props.estimatedDistance
          }
        }
  in
    fareBreakUpConfig'

whereToButtonConfig :: ST.HomeScreenState -> PrimaryButton.Config
whereToButtonConfig state =
  let
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig
        { text = (getString WHERE_TO)
        , color = Color.yellow900     
        , textSize = FontSize.a_16
        , width = MATCH_PARENT
        , gravity = LEFT
        }
      , height = V 60
      , gravity = CENTER
      , cornerRadius = 8.0
      , background = Color.black900
      , margin = (MarginHorizontal 16 16)  
      , isClickable = true 
      , isPrefixImage = true
      , prefixImageConfig
        { imageUrl = "ny_ic_bent_right_arrow,https://assets.juspay.in/nammayatri/images/user/ny_ic_bent_right_arrow.png"
        , height = V 16
        , width = V 21
        , margin = (Margin 17 0 17 0)  
        }
      , id = "WheretoButton"
      }
  in primaryButtonConfig'

primaryButtonRequestRideConfig :: ST.HomeScreenState -> PrimaryButton.Config
primaryButtonRequestRideConfig state =
  let
    config = PrimaryButton.config
    primaryButtonConfig' =
      config
        { textConfig
          { text = (getString REQUEST_RIDE)
          , color = Color.yellow900
          , textSize = FontSize.a_16
          }
        , background = Color.black900
        , margin = (Margin 0 32 0 15)
        , id = "RequestRideButton"
        , enableLoader = (JB.getBtnLoader "RequestRideButton")
        }
  in
    primaryButtonConfig'

primaryButtonConfirmPickupConfig :: ST.HomeScreenState -> PrimaryButton.Config
primaryButtonConfirmPickupConfig state =
  let
    config = PrimaryButton.config
    primaryButtonConfig' =
      config
        { textConfig
          { text = (getString CONFIRM_LOCATION)
          , color = Color.yellow900
          , textSize = FontSize.a_16
          }
        , background = Color.black900
        , margin = (Margin 0 22 0 0)
        , id = "ConfirmLocationButton"
        }
  in
    primaryButtonConfig'

rateRideButtonConfig :: ST.HomeScreenState -> PrimaryButton.Config
rateRideButtonConfig state =
  let
    config = PrimaryButton.config
    primaryButtonConfig' =
      config
        { textConfig
          { text = (getString RATE_YOUR_DRIVER)
          , color = Color.yellow900
          , textSize = FontSize.a_16
          , fontStyle = FontStyle.bold LanguageStyle
          }
        , background = Color.black900
        , margin = (MarginLeft 12)
        , id = "RateYourDriverButton"
        , enableLoader = (JB.getBtnLoader "RateYourDriverButton")
        }
  in
    primaryButtonConfig'

cancelRidePopUpConfig :: ST.HomeScreenState -> CancelRidePopUpConfig.Config
cancelRidePopUpConfig state =
  let
    cancelRideconfig = CancelRidePopUpConfig.config
    lastIndex = ((DA.length state.props.cancellationReasons) - 1)
    cancelRideconfig' =
      cancelRideconfig
        { cancelRideReasons = state.props.cancellationReasons
        , primaryButtonTextConfig
          { firstText = (getString GO_BACK_)
          , secondText = (getString CANCEL_RIDE)
          }
        , activeIndex = state.props.cancelRideActiveIndex
        , activeReasonCode = Just state.props.cancelReasonCode
        , isLimitExceeded = ((DS.length (state.props.cancelDescription)) >= 100)
        , isCancelButtonActive =
          ( case state.props.cancelRideActiveIndex of
              Just cancelRideIndex -> true
              Nothing -> false
          )
        , headingText = ((getString CANCEL_RIDE) <> "?")
        , subHeadingText = (getString PLEASE_TELL_US_WHY_YOU_WANT_TO_CANCEL)
        , hint = (getString HELP_US_WITH_YOUR_REASON)
        , strings
          { mandatory = (getString MANDATORY)
          , limitReached = ((getString MAX_CHAR_LIMIT_REACHED) <> " 100 " <> (getString OF) <> " 100")
          }
        }
  in
    cancelRideconfig'

logOutPopUpModelConfig :: ST.HomeScreenState -> PopUpModal.Config
logOutPopUpModelConfig state =
  if state.props.isPopUp == ST.Logout then
    let
      config' = PopUpModal.config
      popUpConfig' =
        config'
          { primaryText { text = (getString LOGOUT_) }
          , secondaryText { text = (getString ARE_YOU_SURE_YOU_WANT_TO_LOGOUT) }
          , option1 { text = (getString GO_BACK_) }
          , option2 { text = (getString LOGOUT_) }
          }
    in
      popUpConfig'
  else
    let
      config' = PopUpModal.config
      popUpConfig' =
        config'
          { primaryText { text = (getString CANCEL_) }
          , secondaryText { text = if state.props.isPopUp == ST.ActiveQuotePopUp then (getString YOU_HAVE_RIDE_OFFERS_ARE_YOU_SURE_YOU_WANT_TO_CANCEL) else (getString ARE_YOU_SURE_YOU_WANT_TO_CANCEL)}
          , option1 { text = (getString NO) }
          , option2 { text = (getString YES) }
          }
    in
      popUpConfig'

distanceOusideLimitsConfig :: ST.HomeScreenState -> PopUpModal.Config
distanceOusideLimitsConfig state =
  let
    config' = PopUpModal.config
    popUpConfig' =
      config'
        { backgroundClickable = false
        , primaryText
          { text = (getString DESTINATION_OUTSIDE_LIMITS)
          , margin = (Margin 16 20 16 0)
          , fontSize = FontSize.a_20
          }
        , secondaryText
          { text = (getString DROP_LOCATION_FAR_AWAY)
          , margin = (Margin 0 16 0 20)
          }
        , option1 { visibility = false }
        , option2
          { text = (getString CHANGE_DROP_LOCATION)
          , margin = (Margin 16 0 16 EHC.safeMarginBottom)
          }
        }
  in
    popUpConfig'

shortDistanceConfig :: ST.HomeScreenState -> PopUpModal.Config
shortDistanceConfig state =
  let
    config' = PopUpModal.config
    popUpConfig' =
      config'
        { backgroundClickable = false
        , primaryText
          { text = (getString YOUR_TRIP_IS_TOO_SHORT_YOU_ARE_JUST) <> HU.toString (state.props.distance) <> (getString METERS_AWAY_FROM_YOUR_DESTINATION)
          , margin = (Margin 16 20 16 0)
          , fontSize = FontSize.a_20
          }
        , secondaryText
          { text = (getString YOU_CAN_TAKE_A_WALK_OR_CONTINUE_WITH_RIDE_BOOKING)
          , margin = (Margin 0 16 0 20)
          }
        , option1 { text = (getString GO_BACK_) }
        , option2 { text = (getString BOOK_RIDE_) }
        }
  in
    popUpConfig'

sourceUnserviceableConfig :: ST.HomeScreenState -> ErrorModal.Config
sourceUnserviceableConfig state =
  let
    config = ErrorModal.config
    errorModalConfig' =
      config
        { height = WRAP_CONTENT
        , gravity = BOTTOM
        , background = Color.white900
        , stroke = ("1," <> Color.borderGreyColor)
        , corners = (Corners 20.0 true true false false)
        , imageConfig
          { imageUrl = "ny_ic_location_unserviceable,https://assets.juspay.in/nammayatri/images/user/ny_ic_location_unserviceable.png"
          , height = V 99
          , width = V 133
          , margin = (Margin 0 50 0 20)
          }
        , errorConfig
          { text = (getString LOCATION_UNSERVICEABLE)
          , textSize = FontSize.a_18
          , color = Color.black800
          , margin = (MarginBottom 5)
          , fontStyle = FontStyle.bold LanguageStyle
          }
        , errorDescriptionConfig
          { text = (getString CURRENTLY_WE_ARE_LIVE_IN_)
          , color = Color.black700
          , textSize = FontSize.a_14
          , margin = (Margin 20 0 20 (40 + EHC.safeMarginBottom))
          , fontStyle = FontStyle.regular LanguageStyle
          }
        , buttonConfig
          { text = (getString CHANGE_LOCATION)
          , textSize = FontSize.a_16
          , margin = (Margin 16 0 16 (20 + EHC.safeMarginBottom))
          , fontStyle = FontStyle.medium LanguageStyle
          , background = Color.black900
          , color = Color.yellow900
          }
        }
  in
    errorModalConfig'

rateCardConfig :: ST.HomeScreenState -> RateCard.Config
rateCardConfig state =
  let
    config' = RateCard.config
    rateCardConfig' =
      config'
        { baseFare = "₹" <> HU.toString (state.data.rateCard.baseFare)
        , extraFare = "₹" <> HU.toString (state.data.rateCard.extraFare)
        , nightCharges = state.data.rateCard.nightCharges
        , pickUpCharges = "₹" <> HU.toString (state.data.rateCard.pickUpCharges)
        , additionalFare = "₹" <> HU.toString (state.data.rateCard.additionalFare)
        , nightShiftMultiplier = HU.toString (state.data.rateCard.nightShiftMultiplier)
        }
  in
    rateCardConfig'

estimateChangedPopupConfig :: ST.HomeScreenState -> PopUpModal.Config
estimateChangedPopupConfig state =
  let
    config' = PopUpModal.config
    popUpConfig' =
      config'
        { primaryText { text = (getString ESTIMATES_CHANGED) }
        , secondaryText { text = (getString ESTIMATES_REVISED_TO) <> "₹" <> (show state.data.suggestedAmount) <> "-" <> "₹" <> (show $ (state.data.suggestedAmount + state.data.rateCard.additionalFare)) }
        , option1 { text = (getString GO_HOME_) }
        , option2 { text = (getString CONTINUE) }
        }
  in
    popUpConfig'

driverInfoCardViewState :: ST.HomeScreenState -> DriverInfoCard.DriverInfoCardState
driverInfoCardViewState state = { props:
                                  { currentStage: state.props.currentStage
                                  , trackingEnabled: state.props.isInApp
                                  }
                              , data: state.data.driverInfoCardState
                            }

emergencyHelpModelViewState :: ST.HomeScreenState -> EmergencyHelp.EmergencyHelpModelState
emergencyHelpModelViewState state = { showContactSupportPopUp: state.props.showContactSupportPopUp
                                , showCallPolicePopUp: state.props.showCallPolicePopUp
                                , emergencyContactData: state.props.emergencyContactData
                                }

ratingCardViewState :: ST.HomeScreenState -> RatingCard.RatingCardState
ratingCardViewState state = { props:
                            { currentStage: state.props.currentStage
                            , estimatedDistance: state.props.estimatedDistance
                            , showFareBreakUp: false
                            , enableFeedback: true
                            }
                        , data: state.data.previousRideRatingState
                        }

searchLocationModelViewState :: ST.HomeScreenState -> SearchLocationModel.SearchLocationModelState
searchLocationModelViewState state = { isSearchLocation: state.props.isSearchLocation
                                    , locationList: state.data.locationList
                                    , source: state.data.source
                                    , destination: state.data.destination
                                    , isSource: state.props.isSource
                                    , isSrcServiceable: state.props.isSrcServiceable
                                    , isDestServiceable: state.props.isDestServiceable
                                    , isRideServiceable: state.props.isRideServiceable
                                    , savedlocationList: state.data.savedLocations
                                    }

quoteListModelViewState :: ST.HomeScreenState -> QuoteListModel.QuoteListModelState
quoteListModelViewState state = { source: state.data.source
                            , destination: state.data.destination
                            , quoteListModel: state.data.quoteListModelState
                            , selectedQuote: state.props.selectedQuote
                            , autoSelecting: state.props.autoSelecting
                            , searchExpire: state.props.searchExpire
                            }

previousRideRatingViewState :: ST.HomeScreenState -> RatingCard.RatingCardState
previousRideRatingViewState state = { props:
                                        { currentStage: state.props.currentStage
                                        , estimatedDistance: state.props.estimatedDistance
                                        , enableFeedback: false
                                        , showFareBreakUp: true
                                        }
                                    , data: state.data.previousRideRatingState
                                    }

rideRequestAnimConfig :: AnimConfig.AnimConfig
rideRequestAnimConfig =
  let
    config = AnimConfig.animConfig
    rideRequestAnimConfig' =
      config
        { duration = 300
        , fromY = 10
        }
  in
    rideRequestAnimConfig'

rideCompletedAnimConfig :: AnimConfig.AnimConfig
rideCompletedAnimConfig =
  let
    config = AnimConfig.animConfig
    rideCompletedAnimConfig' =
      config
        { duration = 400
        , fromScaleY = 2.5
        , toScaleX = 1.0
        , fromScaleX = 2.5
        , toScaleY = 1.0
        }
  in
    rideCompletedAnimConfig'

autoAnimConfig :: AnimConfig.AnimConfig
autoAnimConfig =
  let
    config = AnimConfig.animConfig
    autoAnimConfig' =
      config
        { duration = 400
        , toScaleX = 1.0
        , toScaleY = 1.0
        }
  in
    autoAnimConfig'