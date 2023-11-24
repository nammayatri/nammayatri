module Components.StepsHeaderModal.Controller where

-- import Merchant.Utils as MU
import MerchantConfig.Utils as MU
import Common.Types.App (LazyCheck(..)) as Lazy
import Data.Maybe (Maybe(..))
import Language.Strings (getString)

data Action = OnArrowClick | Logout

type Config = {
  activeIndex :: Int,
  customerTextArray :: Array String,
  driverTextArray :: Array String,
  numberOfSteps :: Int,
  backArrowVisibility :: Boolean,
  stepsViewVisibility :: Boolean,
  logoutVisibility :: Boolean,
  profileIconVisibility :: Boolean,
  driverNumberVisibility :: Boolean,
  driverMobileNumber :: Maybe String,
  rightButtonText :: String
}

config :: Int -> Config
config currentIndex = {
    activeIndex : currentIndex,
    customerTextArray : ["Let’s get you trip-ready!", "Got an OTP?", "Just one last thing"],
    driverTextArray : ["Let’s get you trip-ready!", "Got an OTP?", "Registration", "Driver Licence Details", "Upload Driving Licence", "Vehicle Registration Details" , "Upload Registration Certificate" , "Grant Permissions"],
    numberOfSteps : 3,
    backArrowVisibility : true, --MU.showCarouselScreen Lazy.FunctionCall
    stepsViewVisibility : true,
    logoutVisibility : false,
    profileIconVisibility : false,
    driverNumberVisibility : false,
    driverMobileNumber : Just "",
    rightButtonText : "logout"
}