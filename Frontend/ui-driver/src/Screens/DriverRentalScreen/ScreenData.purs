module Screens.DriverRentalScreen.ScreenData where

import Prelude

import Common.Types.App (CheckBoxOptions, LazyCheck(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Foreign.Object (empty)
import MerchantConfig.Utils (getValueFromConfig)
import Prelude (class Eq, unit, (<>), (==), (||), (/=))
import Screens.Types (RentalRequestDetial(..), DriverRentalScreenState(..) , RentalRequestDetial , DriverRentalScreenStateData , DriverRentalScreenStateProps(..))
import Services.API (GetDriverInfoResp(..), OrganizationInfo(..))
import Screens.Types


initData :: DriverRentalScreenState
initData = {
    data :{ 
        rentalRequestDetails : [
                         {  sourceArea : "",
                            sourceAddress : "",
                            sourcePincode : "",
                            time : "",
                            distance : "",
                            baseFare : "",
                            pickupDistance : "",
                            pickupTime :  ""
                        }
        ]
        , screenName : "Rental Screen"
      }
    ,
    props : {
        isRentalAccepted : false
    }
}

                    

