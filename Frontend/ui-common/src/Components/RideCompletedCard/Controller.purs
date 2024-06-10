module Components.RideCompletedCard.Controller where

import Prelude

import Components.Banner as Banner
import Components.PrimaryButton.Controller as PB
import Components.SelectListModal.Controller as SL
import Constants.Configs(dummyPrice)
import Data.Int (toNumber)
import Helpers.Utils (parseFloat)
import PrestoDOM 
import Prim.TypeError as String
import Common.Styles.Colors as Color
import Font.Style (Style (..))
import Components.PopUpModal as PopUpModal
import Halogen.VDom.DOM.Prop (Prop)
import Common.Types.App 
import Font.Style as FontStyle
import Styles.Types (FontStyle)
import Data.Eq.Generic (genericEq)
import Foreign.Generic (class Decode, class Encode)
import Data.Generic.Rep (class Generic)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode, defaultEnumDecode, defaultEnumEncode)
import Data.Maybe
import Styles.Types
import Common.Types.App (RentalBookingConfig)
import CarouselHolder as CarouselHolder
import PrestoDOM.List
import Halogen.VDom.DOM.Prop
import Debug
import Data.Maybe (Maybe(..))
import Engineering.Helpers.Commons 

data Action = Support
            | RideDetails
            | SelectButton Boolean Int
            | RateClick Int
            | SkipButtonActionController PB.Action
            | ContactSupportPopUpAC PopUpModal.Action
            | UpiQrRendered String
            | BannerAction Banner.Action 
            | HelpAndSupportAC
            | GoToSOS
            | NoAction
            | BannerChanged String
            | BannerMoving String


type RentalRideTextConfig = {
  rideTime :: String,
  rideDistance :: String,
  rideStart :: String,
  rideStartedAt :: String,
  rideEnd :: String,
  rideEndedAt :: String,
  odometerReading :: String
}

type Config = {
  isDriver :: Boolean,
  topCard :: TopCard,
  customerIssue :: CustomerIssue,
  customerBottomCard :: CustomerBottomCard,
  driverBottomCard :: DriverBottomCard,
  contactSupportPopUpConfig :: PopUpModal.Config,
  badgeCard :: BadgeCard,
  showContactSupportPopUp :: Boolean,
  driverUpiQrCard :: DriverUpiQrCard,
  noVpaCard :: NoVpaCard,
  primaryButtonConfig :: PB.Config,
  accessibility :: Accessiblity,
  theme :: Theme,
  isPrimaryButtonSticky :: Boolean,
  bannerConfig :: Banner.Config,
  viewsByOrder :: Array RideCompletedElements,
  enableContactSupport :: Boolean,
  isFreeRide :: Boolean,
  needHelpText :: String,
  lottieQRAnim :: LottieQRAnim,
  showSafetyCenter :: Boolean,
  safetyTitle :: String,
  rentalRideConfig :: RentalRideConfig,
  rentalRideTextConfig :: RentalRideTextConfig,
  capacity :: Maybe Int,
  serviceTierAndAC :: String,
  additionalCharges :: Array AdditionalCharges,
  rentalRowDetails :: RentalRowConfig,
  rentalBookingData :: RentalBookingConfig,
  showRentalRideDetails :: Boolean,
  bottomBackground :: String
}

data Theme = DARK | LIGHT

derive instance genericTheme :: Generic Theme _
instance decodeTheme :: Decode Theme where decode = defaultEnumDecode
instance encodeTheme :: Encode Theme where encode = defaultEnumEncode
instance eqTheme :: Eq Theme where eq = genericEq

data RideCompletedElements = BANNER | QR_VIEW | NO_VPA_VIEW | BADGE_CARD | DRIVER_BOTTOM_VIEW | RENTAL_RIDE_VIEW

derive instance genericRideCompletedElements :: Generic RideCompletedElements _
instance eqRideCompletedElements :: Eq RideCompletedElements where eq = genericEq

data RentalRowView = RideTime | RideDistance | RideStartedAt | RideEndedAt | EstimatedFare | ExtraTimeFare | ExtraDistanceFare | TotalFare | Surcharges

derive instance genericRentalRowView :: Generic RentalRowView _
instance eqRentalRowView :: Eq RentalRowView where eq = genericEq

type RentalTextConfig = {
  title :: String,
  subTitle :: String,
  estimatedValue :: String,
  actualValue :: String,
  color :: String
}

config :: Config 
config = {
  isDriver : true,
  isFreeRide : false,
  capacity : Nothing,
  serviceTierAndAC : "",
  showSafetyCenter : false,
  topCard : {
    title : "",
    titleColor : Color.grey900,
    finalAmount : 0,
    finalAmountWithCurrency : dummyPrice,
    initialAmount : 0,
    initialAmountWithCurrency : dummyPrice,
    fareUpdatedVisiblity : false,
    gradient : [Color.black900, Color.black900, Color.pickledBlue, Color.black900],
    topPill : {
      text : "",
      background : Color.black900,
      textColor : Color.white900,
      visible : false,
      icon : Nothing
    },
    infoPill : {
      image : "",
      imageVis : GONE,
      text :  "",
      color : Color.black600,
      background : Color.transparent,
      stroke : "1," <> Color.black700,
      cornerRadius : 8.0, 
      padding :  Padding 16 12 16 12,
      margin : Margin 15 16 15 0,
      alpha : 1.0,
      fontStyle : Tags,
      visible : VISIBLE
    },
    bottomText : "",
    horizontalLineColor : Color.white900
  }
, customerIssue: {
    currentIndex : 0
  , currentPageIndex : 0
  , bannerComputedView : Nothing
  , customerIssueCards : []
  , showIssueBanners : true

  }
, customerBottomCard : {
    visible : false,
    title : "",
    subTitle : "",
    driverImage : "",
    selectedRating : 0
  },
  driverBottomCard : {
    visible : false,
    savedMoney : []
  },
  badgeCard : {
    visible : false,
    background : Color.white900,
    text1 : "",
    text1Color : Color.darkCharcoal,
    text2 : "",
    text2Color : Color.darkCharcoal,
    image : "",
    imageWidth : V 0, 
    imageHeight : V 0,
    stroke : "1," <> Color.grey900
  },
  driverUpiQrCard : {
    text : "",
    id : "",
    vpa : "",
    vpaIcon : "",
    collectCashText : ""
  },
  noVpaCard : {
    title : "",
    collectCashText : ""
  },
  contactSupportPopUpConfig : PopUpModal.config,
  showContactSupportPopUp : false,
  primaryButtonConfig : PB.config,
  accessibility : ENABLE,
  theme : DARK,
  isPrimaryButtonSticky : false,
  bannerConfig : Banner.config,
  viewsByOrder : [],
  rentalRideConfig : {
    showRideOdometerReading : false,
    rideStartODOReading : "",
    rideEndODOReading : "",
    baseRideDuration : "",
    baseRideDistance : "",
    actualRideDuration : "",
    actualRideDistance : "",
    startRideOdometerImage: "",
    endRideOdometerImage: "",
    rideStartedAt : "",
    rideEndedAt : ""
  },
  rentalRideTextConfig : {
    rideTime : "Ride Time",
    rideDistance : "Ride Distance",
    rideStart : "Ride Start",
    rideStartedAt : "Ride Started At",
    rideEnd : "Ride End",
    rideEndedAt : "Ride Ended At",
    odometerReading : "Odometer Reading"
  },
  enableContactSupport : true,
  lottieQRAnim : {
    visible : false,
    url : ""
  },
  needHelpText : "",
  safetyTitle : "",
  additionalCharges : [],
  bottomBackground : Color.grey700,
  rentalRowDetails : dummyRentalRowConfig,
  rentalBookingData : dummyRentalBookingConfig,
  showRentalRideDetails : false
}

type CustomerIssue = {
  currentIndex :: Int
, currentPageIndex :: Int
, bannerComputedView :: Maybe ListItem
, customerIssueCards :: Array CustomerIssueCard
, showIssueBanners :: Boolean

}

type CustomerIssueCard = {
  selectedYes :: Maybe Boolean,
  title :: String,
  subTitle :: String,
  yesText :: String,
  noText :: String
, issueType :: CustomerIssueTypes
}


type TopCard = {
  title :: String,
  titleColor :: String,
  finalAmount :: Int,
  finalAmountWithCurrency :: Price,
  initialAmount :: Int,
  initialAmountWithCurrency :: Price,
  fareUpdatedVisiblity :: Boolean,
  gradient :: Array String,
  topPill :: TopPill,
  infoPill :: InfoPill, 
  bottomText :: String,
  horizontalLineColor :: String
}

type InfoPill = {
  image :: String ,
  imageVis :: Visibility, 
  text :: String ,
  color :: String ,
  background :: String ,
  stroke :: String, 
  cornerRadius :: Number, 
  padding ::  Padding,
  margin :: Margin,
  alpha :: Number,
  fontStyle :: Style,
  visible :: Visibility
}

type CustomerBottomCard = {
  visible :: Boolean,
  title :: String,
  subTitle :: String,
  driverImage :: String, 
  selectedRating :: Int
}

type DriverBottomCard = {
  visible :: Boolean,
  savedMoney :: Array SavedMoney
}

type SavedMoney = {
  amount :: Int
, reason :: String 
}

type BadgeCard = {
  visible :: Boolean,
  background :: String,
  text1 :: String,
  text1Color :: String,
  text2 :: String,
  text2Color :: String,
  image :: String,
  imageWidth :: Length, 
  imageHeight :: Length,
  stroke :: String
}

type DriverUpiQrCard = {
  text :: String,
  id :: String,
  vpa :: String,
  vpaIcon :: String,
  collectCashText :: String
}

type NoVpaCard = {
  title :: String,
  collectCashText :: String
}

type TopPill = {
  visible :: Boolean,
  background :: String,
  text :: String,
  textColor :: String,
  icon :: Maybe String
}

type LottieQRAnim = {
  visible :: Boolean,
  url :: String
}

type InfoCardConfig = {
  height :: Length,
  width :: Length,
  margin :: Margin,
  image :: InfoCardImageConfig , 
  heading :: InfocardTextConfig,
  headingInfo :: InfocardTextConfig,
  subHeading1 :: InfocardTextConfig,
  subHeading2 :: InfocardTextConfig,
  id :: String
}

type InfoCardImageConfig = {
  width :: Length,
  height :: Length,
  visibility :: Visibility,
  renderImage :: String
}

type InfocardTextConfig = {
  text :: String,
  color :: String,
  fontStyle :: forall properties. (Array (Prop properties)),
  visibility :: Visibility
}

type RentalRideConfig = {
  showRideOdometerReading :: Boolean,
  rideStartODOReading :: String,
  rideEndODOReading :: String,
  baseRideDuration :: String,
  baseRideDistance :: String,
  actualRideDuration :: String,
  actualRideDistance :: String,
  startRideOdometerImage:: String,
  endRideOdometerImage:: String,
  rideStartedAt :: String,
  rideEndedAt :: String
}

type AdditionalCharges = {
  text :: String
, visibility :: Visibility
, textColor :: Color
, image :: String
}

type RentalRowConfig = {
    rideTime :: String
  , rideDistance :: String
  , rideDistanceInfo :: String
  , rideStartedAt :: String
  , rideEndedAt :: String
  , estimatedFare :: String
  , extraTimeFare :: String
  , extraDistanceFare :: String
  , totalFare :: String
  , rideDetailsTitle :: String
  , fareUpdateTitle :: String
  , surcharges :: String
}

dummyRentalRowConfig :: RentalRowConfig
dummyRentalRowConfig = {
    rideTime : ""
  , rideDistance : ""
  , rideDistanceInfo : ""
  , rideStartedAt : ""
  , rideEndedAt : ""
  , estimatedFare : ""
  , extraTimeFare : ""
  , extraDistanceFare : ""
  , totalFare : ""
  , rideDetailsTitle : ""
  , fareUpdateTitle : ""
  , surcharges : ""
}

dummyRentalBookingConfig :: RentalBookingConfig
dummyRentalBookingConfig = 
  { startTimeUTC : ""
  , baseDuration : 0
  , baseDistance : 0
  , startOdometer : ""
  , endOdometer : ""
  , nightCharge : ""
  , finalDuration : 0
  , finalDistance : 0
  , rideStartedAt : "" 
  , rideEndedAt : ""
  , extraDistanceFare : ""
  , extraTimeFare : ""
  }




  

type IssueReportBannerProps =(
  layoutStroke :: PropValue
, cornerRadius :: PropValue
, background :: PropValue
, visibility :: PropValue
, title :: PropValue
, titleColor :: PropValue
, subTitle :: PropValue
, subTitleColor :: PropValue
, yesBackground :: PropValue
, yesText :: PropValue
, yesTextColor :: PropValue
, yesStroke :: PropValue
, noBackground :: PropValue
, noText :: PropValue
, noTextColor :: PropValue
, noStroke :: PropValue
)

issueReportBannersTransformer  :: Array CustomerIssueCard -> Array (Record IssueReportBannerProps)
issueReportBannersTransformer arr =
  let _ = spy "ARRRR" arr
  in map (
  \item -> {
    layoutStroke : toPropValue $ "1," <> Color.grey800,
    background : toPropValue Color.white900,
    visibility : toPropValue "visible",
    title : toPropValue item.title,
    titleColor : toPropValue Color.black800,
    subTitle : toPropValue item.subTitle,
    subTitleColor : toPropValue Color.black700,
    yesBackground : toPropValue if item.selectedYes == Just true then Color.blue600 else Color.white900,
    yesText : toPropValue item.yesText,
    yesStroke : toPropValue $ if item.selectedYes == Just true then Color.blue900 else Color.grey800,
    yesTextColor : toPropValue Color.black800,
    noBackground : toPropValue if item.selectedYes == Just false then Color.blue600 else Color.white900,
    noText : toPropValue item.noText,
    noTextColor : toPropValue Color.black800,
    noStroke : toPropValue $ if item.selectedYes == Just false then Color.blue900 else Color.grey800,
    cornerRadius : toPropValue if os == "IOS" then "12.0" else "20.0"
  }
) arr


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