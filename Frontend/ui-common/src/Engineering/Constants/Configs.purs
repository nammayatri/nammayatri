module Constants.Configs where 

import Common.Types.App (PolylineAnimationConfig, Price(..), Currency(..), Distance(..), DistanceUnit(..))
import ConfigProvider
import Common.Styles.Colors (shadowGray)
import Prelude ((==))
import Engineering.Helpers.Commons (os, getCurrencyTypeFromSymbol)

getPolylineAnimationConfig :: PolylineAnimationConfig 
getPolylineAnimationConfig = if os == "IOS" then iosPolylineAnimationConfig else androidPolylineAnimationConfig
  
iosPolylineAnimationConfig :: PolylineAnimationConfig
iosPolylineAnimationConfig = {
    color : shadowGray 
  , draw : 100 -- percentage
  , fade : 100 --percentage
  , delay : 0 -- percentage
  }

androidPolylineAnimationConfig :: PolylineAnimationConfig
androidPolylineAnimationConfig = {
    color : shadowGray
  , draw : 700 -- millisecond
  , fade : 0 -- millisecond
  , delay : 1000 -- millisecond
}

dummyPrice :: Price
dummyPrice = do
  let currencySymobol = getCurrency ""
  {amount: 0.0, currency: getCurrencyTypeFromSymbol currencySymobol}

dummyDistance :: Distance
dummyDistance = Distance {value: 0.0, unit: Meter}