module Components.ChooseVehicle.Controller where

import Prelude (class Eq, class Show, show, (<>) )
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Foreign.Generic (decode, encode, class Decode, class Encode)
import Presto.Core.Utils.Encoding (defaultEnumDecode, defaultEnumEncode, defaultDecode, defaultEncode)
import PrestoDOM (Margin(..))
import Data.Maybe (Maybe(..))
import Common.Types.App (RateCardType(..), FareList)
import Common.Types.App as CT

instance showAction :: Show Action where
  show (NoAction var1) = "NoAction_" <> show var1
  show (OnSelect var1) = "OnSelect_" <> show var1
  show (OnImageClick) = "OnImageClick"
  show (ShowRateCard var1) = "ShowRateCard_" <> show var1
  show (OnEditClick) = "OnEditClick"
  show (ServicesOnClick var1 var2) = "ServicesOnClick_" <> show var1 <> "_" <> show var2

data Action
  = NoAction Config
  | OnSelect Config
  | OnImageClick
  | ShowRateCard Config
  | OnEditClick
  | ServicesOnClick Config String

type Config
  = { vehicleImage :: String
    , isSelected :: Boolean
    , vehicleVariant :: String
    , vehicleType :: String
    , capacity :: String
    , price :: String
    , isCheckBox :: Boolean
    , isEnabled :: Boolean
    , index :: Int
    , activeIndex :: Int
    , id :: String
    , maxPrice :: Maybe Int
    , minPrice :: Maybe Int
    , basePrice :: Int
    , showInfo :: Boolean
    , searchResultType :: SearchResultType
    , isBookingOption :: Boolean
    , pickUpCharges :: Number 
    , tollCharge :: Number
    , serviceTierShortDesc :: Maybe String
    , serviceTierName :: Maybe String
    , extraFare :: Array FareList
    , additionalFare :: Int
    , driverAdditions :: Array FareList
    , fareInfoDescription :: Array String
    , isNightShift :: Boolean
    , nightChargeTill :: String
    , nightChargeFrom :: String
    , airConditioned :: Maybe Boolean
    , showEditButton :: Boolean
    , editBtnText :: String
    , layoutMargin :: Margin 
    , providerName :: String
    , providerId :: String
    , providerType :: CT.ProviderType
    , singleVehicle :: Boolean
    , priceShimmer :: Boolean
    , availableServices :: Array String
    , services :: Array String
    , selectedServices :: Array String
    , currentEstimateHeight :: Int
    , selectedEstimateHeight :: Int
    , validTill :: String
    , waitingTimeInfo :: CT.WaitingTimeInfo
    , showStroke :: Boolean
    , hasTollCharges :: Boolean
    , hasParkingCharges :: Boolean
    , smartTipSuggestion :: Maybe Int
    , specialLocationTag :: Maybe String
    , smartTipReason :: Maybe String
    , enableOffer :: Boolean
    , actualPrice :: String
    }

data SearchResultType = QUOTES FareProductType | ESTIMATES

derive instance genericSearchResultType :: Generic SearchResultType _
instance eqSearchResultType :: Eq SearchResultType where eq = genericEq
instance showSearchResultType :: Show SearchResultType where show = genericShow
instance encodeSearchResultType :: Encode SearchResultType where encode = defaultEncode
instance decodeSearchResultType:: Decode SearchResultType where decode = defaultDecode

data FareProductType =  ONE_WAY
                      | INTER_CITY
                      | RENTAL
                      | DRIVER_OFFER
                      | OneWaySpecialZoneAPIDetails
                      | DELIVERY
                      | AMBULANCE

derive instance genericFareProductType :: Generic FareProductType _
instance showFareProductType :: Show FareProductType where show = genericShow
instance eqFareProductType :: Eq FareProductType where eq = genericEq
instance encodeFareProductType :: Encode FareProductType where encode = defaultEncode
instance decodeFareProductType :: Decode FareProductType where decode = defaultDecode


config :: Config
config =
  { vehicleImage: ""
  , isSelected: false
  , vehicleVariant: ""
  , vehicleType: ""
  , capacity: ""
  , price: ""
  , isCheckBox: false
  , isEnabled: true
  , activeIndex: 0
  , index: 0
  , id: ""
  , maxPrice : Nothing
  , minPrice : Nothing
  , basePrice : 0 
  , showInfo : false
  , searchResultType : ESTIMATES
  , isBookingOption : false
  , pickUpCharges : 0.0
  , layoutMargin : MarginHorizontal 0 0
  , tollCharge : 0.0
  , serviceTierShortDesc : Nothing
  , serviceTierName : Nothing
  , extraFare: []
  , additionalFare: 0
  , fareInfoDescription: []
  , driverAdditions: []
  , isNightShift : false
  , nightChargeTill : ""
  , nightChargeFrom : ""
  , airConditioned : Nothing
  , showEditButton : false
  , editBtnText : ""
  , providerName : ""
  , providerId : ""
  , providerType : CT.ONUS
  , singleVehicle : false
  , priceShimmer : true
  , availableServices : []
  , services : [] 
  , selectedServices : []
  , currentEstimateHeight : 184 
  , selectedEstimateHeight : 0
  , validTill : ""
  , waitingTimeInfo : { freeMinutes: "", charge: "" }
  , showStroke : true
  , hasTollCharges : false 
  , hasParkingCharges : false
  , specialLocationTag : Nothing
  , smartTipSuggestion: Nothing
  , smartTipReason : Nothing
  , enableOffer : false
  , actualPrice : ""
  }
