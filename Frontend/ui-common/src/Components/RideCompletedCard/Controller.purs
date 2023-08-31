module Components.RideCompletedCard.Controller where

import Prelude

import Components.PrimaryButton.Controller as PB
import Components.SelectListModal.Controller as SL
import Data.Int (toNumber)
import Helpers.Utils (parseFloat)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), Accessiblity(..), afterRender, background, clickable, color, cornerRadius, fontStyle, gravity, height, id, imageView, imageWithFallback, letterSpacing, lineHeight, linearLayout, margin, onClick, orientation, padding, scrollView, stroke, text, textSize, textView, visibility, weight, width, onAnimationEnd)
import Prim.TypeError as String
import Common.Styles.Colors as Color
import Font.Style (Style (..))
import Components.PopUpModal as PopUpModal

data Action = Support
            | RideDetails
            | SelectButton Int
            | IssueReportIndex Int
            | RateClick Int
            | IssueReportPopUpAC SL.Action
            | SkipButtonActionController PB.Action
            | ContactSupportPopUpAC PopUpModal.Action
            
type Config = {
  topCard :: TopCard,
  customerIssueCard :: CustomerIssueCard,
  customerBottomCard :: CustomerBottomCard,
  driverBottomCard :: DriverBottomCard,
  contactSupportPopUpConfig :: PopUpModal.Config,
  badgeCard :: BadgeCard,
  showContackSupportPopUp :: Boolean,
  primaryButtonConfig :: PB.Config,
  accessibility :: Accessiblity
}

config :: Config 
config = {
  topCard : {
    title : "",
    finalAmount : 0,
    initalAmount : 0,
    fareUpdatedVisiblity : false,
    topPill : {
      text : "",
      background : Color.black900,
      textColor : Color.white900,
      visible : false
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
      margin : MarginVertical 10 20,
      alpha : 1.0,
      fontStyle : Tags
    },
    bottomText : ""
  },
  customerIssueCard : {
    issueFaced : false, 
    reportIssueView : false,
    selectedYesNoButton : 0,
    reportIssuePopUpConfig : SL.config,
    title : "",
    subTitle : "",
    option1Text : "",
    option2Text : "",
    yesText : "",
    noText : ""
  },
  customerBottomCard : {
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
    imageHeight : V 0
  },
  contactSupportPopUpConfig : PopUpModal.config,
  showContackSupportPopUp : false,
  primaryButtonConfig : PB.config,
  accessibility : ENABLE
}

type CustomerIssueCard = {
  issueFaced :: Boolean, 
  reportIssueView :: Boolean,
  selectedYesNoButton :: Int,
  reportIssuePopUpConfig :: SL.Config,
  title :: String,
  subTitle :: String,
  option1Text :: String,
  option2Text :: String,
  yesText :: String,
  noText :: String
}

type TopCard = {
  title :: String,
  finalAmount :: Int,
  initalAmount :: Int,
  fareUpdatedVisiblity :: Boolean,
  topPill :: TopPill,
  infoPill :: InfoPill, 
  bottomText :: String
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
  fontStyle :: Style
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
  imageHeight :: Length
}

type TopPill = {
  visible :: Boolean,
  background :: String,
  text :: String,
  textColor :: String
}