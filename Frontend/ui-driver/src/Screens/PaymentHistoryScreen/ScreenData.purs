{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.PaymentHistoryScreen.ScreenData where

import Common.Types.App (PaymentStatus(..))
import Data.Maybe as Mb
import Screens.Types (PaymentHistoryScreenState, PaymentHistorySubview(..), PaymentListItem, PlanCardConfig, TransactionListItem, PromoConfig)
import Services.API (AutopayPaymentStage(..), FeeType(..))
import Screens.Types as ST
import ConfigProvider

initData :: PaymentHistoryScreenState
initData =
  { data:
      { autoPayList: []
      , manualPayList: []
      , transactionDetails:
          { notificationStatus: Mb.Just NOTIFICATION_SCHEDULED
          , paymentStatus: Pending
          , statusTime: ""
          , details: []
          , manualSpecificDetails: []
          , isSplit: false
          , isAutoPayFailed: false
          , feeType: AUTOPAY_PAYMENT
          , numOfDriverFee: 0
          }
      , planData: dummyPlanConfig
      , gradientConfig: []
      , autoPayStatus: ST.NO_AUTOPAY
      }
  , props:
      { subView: PaymentHistory
      , autoPayHistory: true
      , autoPaySetup: false
      , selectedDue: ""
      , offset: 0
      , enableLoadMore: true
      }
  }

dummyPlanConfig :: PlanCardConfig
dummyPlanConfig =
  { id: ""
  , title: ""
  , description: ""
  , isSelected: false
  , offers:
      [ { title: Mb.Nothing
        , isGradient: true
        , gradient: []
        , hasImage: true
        , imageURL: ""
        , offerDescription: Mb.Nothing
        , addedFromUI: false
        }
      ]
  , priceBreakup: []
  , frequency: ""
  , freeRideCount: 1
  , showOffer: true
  }

dummyPromoConfig :: PromoConfig
dummyPromoConfig =
  { title: Mb.Nothing
  , offerDescription: Mb.Nothing
  , isGradient: false
  , gradient: []
  , hasImage: false
  , imageURL: ""
  , addedFromUI: false
  }
