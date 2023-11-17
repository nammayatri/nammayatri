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
        { fontType: "Assets"
        , currency: "₹"
        , isGradient: "false"
        , gradient: []
        , addFavouriteScreenBackArrow: "ny_ic_chevron_left_white,https://assets.juspay.in/nammayatri/images/user/ny_ic_chevron_left_white.png"
        , popupBackground: "#FFFFFF"
        , apiLoaderLottie: "primary_button_loader.json"
        , primaryTextColor: "#FCC32C"
        , primaryBackground: "#2C2F3A"
        , showCorporateAddress: false
        , imageUploadOptional: true
        , rideCompletedCardConfig : {
            showSavedCommission : false }
        , autoPayBanner: true
        , leaderBoard: { 
            isMaskedName: false
          }
        , black800 : "#454545"
        , black900 : "#2C2F3A"
        , red : "#E55454"
        , subscriptionConfig : {
            enableBlocking : false,
            onBoardingSubscription : false,
            completePaymentPopup : false,
            showLaterButtonforTimeRange : false,
            offerBannerConfig : {
              showDUOfferBanner : false,
              offerBannerValidTill : "",
              offerBannerDeadline : "",
              offerBannerPlans : []
            },
            lowDuesLimit : 25.0,
            maxDuesLimit : 100.0,
            highDueWarningLimit : 75.0,
            moveDriverToOfflineInHighDueDaily : false,
            enableSubscriptionPopups : false,
            supportNumber : "",
            faqLink : "",
            whatsappSupportLink : "",
            myPlanYoutubeLink : "",
            overlayYoutubeLink : "",
            enableIntroductoryView : false,
            optionsMenuItems : {
              managePlan : false,
              paymentHistory : false,
              viewFaqs : false,
              callSupport : false,
              chatSupport : false,
              kioskLocation : false,
              viewAutopayDetails : false
            },
            gradientConfig : [],
            enableSubscriptionSupportPopup : false
        }
        , navigationAppConfig : {
            android : {  
              query : "google.navigation:q=%f,%f"
            , packageName : "com.google.android.apps.maps"
            , walkQuery : "google.navigation:q=%f,%f&mode=w"
            }
          }
        , rideActionModelConfig : {
            showVehicleVariant : true
          }
        , features : {
            enableSuggestions : false
          }
        , referralType : "QRScreen"
        , mapConfig : 
            { animationDuration : 500
            }
        , profile :
            { bookingOptionMenuForTaxi : false
            }
        , purpleRideConfig : {
            showPurpleVideos : false,
            visualImpairmentVideo : "",
            physicalImpairmentVideo : "",
            hearingImpairmentVideo : "",
            genericAccessibilityVideo : ""
          }
        , showPaymentDetails : true
        , gotoConfig : {
            maxGotoLocations : 5,
            enableGoto : false
        }
        , profileVerification : {
          aadharVerificationRequired : false
        } 
        , bottomNavConfig : {
              home : 
                { isVisible : true,
                  showNew : false
                },
              rideHistory : 
                { isVisible : false,
                  showNew : false
                },
              driverEarnings : 
                { isVisible : true,
                  showNew : false
                },
              subscription : 
                { isVisible : false,
                  showNew : false
                },
              referral : 
                { isVisible : true,
                  showNew : false
                },
              notifications :
                { isVisible : true,
                  showNew : false
                }
            }
        , waitTimeConfig : {
          enableWaitTime : true,
          thresholdDist : 0.05,
          thresholdTime : 180
        }
        , coinsConfig : {
            enableYatriCoins : false,
            minCoinSliderValue : 250,
            maxCoinSliderValue : 1500
        }
        , unserviceableThreshold : 250.0
        , cityConfig : [
              {
                cityName : "Bangalore",
                mapImage : "ny_ic_bengalore_map",
                cityCode : "std:080",
                showSubscriptions : true,
                cityLat : 12.971599,
                cityLong : 77.594566,
                supportNumber : "",
                languageKey : "KN_IN"
              },
              {
                cityName : "Hyderabad",
                mapImage : "ny_ic_hyderabad_map",
                cityCode : "std:040",
                showSubscriptions : false,
                cityLat : 17.387140,
                cityLong : 78.491684,
                supportNumber : "+918069724900",
                languageKey : "TE_IN"
              },
              {
                cityName : "Mysore",
                mapImage : "ny_ic_mysuru_map",
                cityCode : "std:0821",
                showSubscriptions : false,
                cityLat : 12.295810,
                cityLong : 76.639381,
                supportNumber : "",
                languageKey : "TA_IN"
              },
              {
                cityName : "Delhi",
                mapImage : "ny_ic_delhi_map",
                cityCode : "std:011",
                showSubscriptions : false,
                cityLat : 28.644800,
                cityLong : 77.216721,
                supportNumber : "+918069724848",
                languageKey : "HI_IN"
              },
              {
                cityName : "Chennai",
                mapImage : "ny_ic_chennai_map",
                cityCode : "std:044",
                showSubscriptions : false,
                cityLat : 13.067439,
                cityLong : 80.237617,
                supportNumber : "08069724899",
                languageKey : "TA_IN"
              },
              {
                cityName : "Coimbatore",
                mapImage : "ny_ic_coimbatore_map",
                cityCode : "std:0422",
                showSubscriptions : false,
                cityLat : 11.004556,
                cityLong : 76.961632,
                supportNumber : "08069724899",
                languageKey : "TA_IN"
              },
              {
                cityName : "Puducherry",
                mapImage : "ny_ic_puducherry_map",
                cityCode : "std:0413",
                showSubscriptions : false,
                cityLat : 11.943852,
                cityLong : 79.808292,
                supportNumber : "",
                languageKey : "TA_IN"
              }--, for future use
              -- {
              --   cityName : "Madurai",
              --   mapImage : "ny_ic_madurai_map",
              --   cityCode : "std:0452",
              --   showSubscriptions : false,
              --   cityLat : 9.93069,
              --   cityLong : 78.11956,
              --   supportNumber : "",
              --   languageKey : "TA_IN"
              -- }
         ]
  }
