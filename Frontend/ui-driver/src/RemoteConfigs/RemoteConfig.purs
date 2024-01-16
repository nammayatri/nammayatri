module RemoteConfigs where

import Prelude
import DecodeUtil (decodeForeignObject, parseJSON)
import Foreign (Foreign)
import Foreign.Index (readProp)
import RemoteConfig.Utils (fetchRemoteConfigString)
import Data.Maybe (Maybe(..))
import Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Presto.Core.Utils.Encoding (defaultDecode)
import Control.Monad.Except (runExcept)


foreign import getSubsRemoteConfig :: String -> Foreign

type RemoteConfig a = {
    bangalore :: a,
    kolkata :: a
}

newtype RCCarousel = RCCarousel {
    text_color:: String,
    text:: String,
    cta_text:: String,
    cta_action:: Maybe String,
    cta_link:: String,
    cta_icon:: String,
    banner_color:: String,
    banner_image:: String,
    cta_background_color:: String,
    cta_text_color:: String,
    cta_corner_radius:: String,
    cta_image_url:: String
}

derive instance genericCarouselConfigItem :: Generic RCCarousel _
instance decodeCarouselConfigItem :: Decode RCCarousel where decode = defaultDecode

type RCSubscription = {
    max_dues_limit :: Number,
    low_dues_warning_limit :: Number,
    high_due_warning_limit :: Number
}

defaultRemoteConfig :: RemoteConfig (Array RCCarousel)
defaultRemoteConfig ={
    bangalore : [],
    kolkata : []
}

subscriptionRemoteConfig :: RCSubscription
subscriptionRemoteConfig = {
    max_dues_limit : 100.0,
    low_dues_warning_limit : 25.0,
    high_due_warning_limit : 75.0
}

subscriptionConfig :: String -> RCSubscription
subscriptionConfig key = do
    let conf = getSubsRemoteConfig $ fetchRemoteConfigString key
    decodeForeignObject conf subscriptionRemoteConfig

carouselConfigData :: String -> String -> Array RCCarousel
carouselConfigData city language = do
    let config = fetchRemoteConfigString ("driver_carousel_banner" <> language)
        value = decodeForeignObject (parseJSON config) defaultRemoteConfig
    getConfig value city

getConfig :: RemoteConfig (Array RCCarousel) -> String -> Array RCCarousel
getConfig config city = 
    case city of
        "bangalore" -> config.bangalore
        "kolkata" -> config.kolkata
        _ -> []
