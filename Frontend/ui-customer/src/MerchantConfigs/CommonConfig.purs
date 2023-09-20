{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module MerchantConfigs.CommonConfig where

import Effect (Effect)
import Foreign (Foreign, unsafeToForeign)
import Prelude (pure, ($))

commonConfig :: Effect Foreign
commonConfig =
  pure
    $ unsafeToForeign
        { showHamMenu: true
        , showQuoteFindingText: false
        , showCorporateAddress: true
        , primaryTextColor: "#FFFFFF"
        , isGradient: "false"
        , gradient: []
        , primaryBackground: "#03B9F5"
        , currency: "₹"
        , alertDialogPrimaryColor: "#00B8F5"
        , primaryButtonCornerRadius: 8.0
        , cancelSearchTextColor: "#FD5154"
        , estimateConfirmText: "Request a NammaYatri Ride"
        , autoConfirmingLoaderColor: "#00B8F5"
        , quoteListModelBackground: "#F5F9FE"
        , ratingConfig:
            { secondaryButtonTextColor: "#00B8F5"
            , secondaryButtonStroke: "1,#00B8F5"
            , buttonCornerRadius: 8.0
            }
        , cancelReasonConfig:
            { secondaryButtonTextColor: "#00B8F5"
            , secondaryButtonStroke: "1,#00B8F5"
            , buttonCornerRadius: 8.0
            }
        , logs: [ "JUSPAY" ]
        , djd: "sdvs"
        , internationalNumberEnabled : false
        , callOptions : ["ANONYMOUS"]
        , autoVariantEnabled : true
        , showNearByDrivers : false
        , navigationAppConfig : {
            android : {  
              query : "google.navigation:q=%f,%f"
            , packageName : "com.google.android.apps.maps"
            , walkQuery : "google.navigation:q=%f,%f&mode=w"
            }
          , ios : {
              query : "http://maps.google.com///?saddr=&daddr=%@,%@&dirflg=d"
            , walkQuery : "http://maps.google.com///?saddr=&daddr=%@,%@&dirflg=w"
            }
          }
        , estimateAndQuoteConfig : 
            { variants :
                { acVariant : ["HATCHBACK", "TAXI_PLUS", "SEDAN", "SUV"]
                , nonAcVariant : ["TAXI"]
                , variantOrder : ["HATCHBACK", "TAXI_PLUS", "SEDAN", "SUV", "TAXI"]
                }
            }
        , mapConfig : 
            { locateOnMapConfig : 
                { dottedLineConfig : 
                    { visible : true
                    , range : 100
                    , color : "#323643"
                    }
                }
            , animationDuration : 400
            }
        }
