module Screens.RideBookingFlow.RiderRideCompletedCard.Config where

import Components.PrimaryButton as PrimaryButton
import Screens.Types (RiderRideCompletedScreenState)
import Language.Strings (getString)
import JBridge as JB
import Language.Types as LT
import Styles.Colors as Color
import PrestoDOM 
import Prelude
import Data.Maybe
import Components.FavouriteDriverInfoCard.Controller as FavouriteDriverInfoCard
import CarouselHolder as CarouselHolder
import Components.RideCompletedCard.Controller (IssueReportBannerProps(..), CustomerIssueCard(..), CustomerIssue(..), issueReportBannersTransformer)
import Screens.RideBookingFlow.RiderRideCompletedCard.Controller (Action(..), issueReportBannerConfigs)
import PrestoDOM.List (ListItem)
import Engineering.Helpers.Commons
import Debug(spy)
import Data.Array as DA
import Common.Types.App as CTP

primaryButtonConfig :: RiderRideCompletedScreenState -> PrimaryButton.Config
primaryButtonConfig state = PrimaryButton.config
    { textConfig
        { text = if state.ratingCard.recordAudioState.recordedFile == Nothing then getString LT.SUBMIT else "Save & Submit"
        , color = Color.yellow900
        , accessibilityHint = "Submit Feedback Button" 
        }
    , alpha = if state.ratingCard.recordAudioState.isRecording == true then 0.5 else 1.0
    , background = Color.black900
    , margin = MarginBottom $ if state.isKeyBoardOpen then 5 else 15
    , id = "RateYourDriverButton"
    , enableLoader = (JB.getBtnLoader "RateYourDriverButton")
    , enableRipple = true
    , rippleColor = Color.rippleShade
    , isClickable = not state.ratingCard.recordAudioState.isRecording
    }

primaryButtonConfigForCarousel :: RiderRideCompletedScreenState -> PrimaryButton.Config
primaryButtonConfigForCarousel state = PrimaryButton.config
    { textConfig
        { text = getString LT.DONE
        , color = Color.yellow900
        , accessibilityHint = "Done with Carousel" 
        }
    , alpha = if (state.customerIssue.buttonActive || isOnlyDemandExtraTollIssue state) then 1.0 else 0.5
    , background = Color.black900
    , margin = (Margin 0 0 0 0)
    , id = "DoneWithCarousel"
    , enableRipple = true
    , rippleColor = Color.rippleShade
    , isClickable = state.customerIssue.buttonActive || isOnlyDemandExtraTollIssue state
    }

isOnlyDemandExtraTollIssue :: RiderRideCompletedScreenState -> Boolean
isOnlyDemandExtraTollIssue state = do
    let bannerConfigs = issueReportBannerConfigs state
    if (DA.length bannerConfigs) == 1 then maybe false (\customerIssue -> customerIssue.issueType == CTP.DemandExtraTollAmount) (bannerConfigs DA.!! 0)
    else false

primaryBtnConfigForRentalTripDetails :: RiderRideCompletedScreenState -> PrimaryButton.Config
primaryBtnConfigForRentalTripDetails state = PrimaryButton.config
    { textConfig
        { text = getString LT.DONE
        , color = Color.yellow900
        , accessibilityHint = "Done : button" 
        }
    , alpha = 1.0
    , background = Color.black900
    , margin = (Margin 0 0 0 0)
    , id = "DoneWithRentalRideDetails"
    , enableRipple = true
    , rippleColor = Color.rippleShade
    , isClickable = true
    }

driverInfoCardConfig :: RiderRideCompletedScreenState -> FavouriteDriverInfoCard.FavouriteDriverInfoCardState
driverInfoCardConfig state = FavouriteDriverInfoCard.config 

getCarouselConfig ∷ ListItem → CustomerIssue → CarouselHolder.CarouselHolderConfig IssueReportBannerProps Action
getCarouselConfig view customerIssueConfig = {
    view
  , items : issueReportBannersTransformer customerIssueConfig.customerIssueCards
  , orientation : HORIZONTAL
  , currentPage: customerIssueConfig.currentPageIndex
  , autoScroll : false
  , autoScrollDelay : 0.0
  , id : "bannerCarousel"
  , autoScrollAction : Nothing 
  , onPageSelected : Just BannerChanged
  , onPageScrollStateChanged : Just BannerMoving
  , onPageScrolled : Nothing
  , currentIndex : customerIssueConfig.currentIndex
  , showScrollIndicator : false
  , layoutHeight : V 180
  , overlayScrollIndicator : false
}