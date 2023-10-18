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
            gradientConfig : []
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
        , referralType : "QRScreen"
        , mapConfig : 
            { animationDuration : 400
            }
        , profile :
            { bookingOptionMenuForTaxi : false
            }
        , enablePurpleRideBanner : false
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
        }
