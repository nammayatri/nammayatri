module Constants.Configs where 

import Common.Types.App (PolylineAnimationConfig)
import Common.Styles.Colors (shadowGray)
import Prelude ((==))
import Engineering.Helpers.Commons (os)

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
  , fade : 1000 -- millisecond
  , delay : 200 -- millisecond
}
