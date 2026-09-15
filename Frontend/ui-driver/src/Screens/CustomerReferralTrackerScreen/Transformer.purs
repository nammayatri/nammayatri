{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.CustomerReferralTrackerScreen.Transformer where

import Prelude
import Screens.CustomerReferralTrackerScreen.Types
import Services.API as API
import Domain.Payments as PP

getDailyEarnings :: Array API.DailyEarnings -> Array DailyEarning
getDailyEarnings dailyEarnings =
  map
    ( \(API.DailyEarnings earning) ->
        { earnings: earning.earnings
        , referrals: earning.referrals
        , earningDate: earning.earningDate
        , activatedItems: earning.activatedItems
        , percentLength: 0.0
        , status : if earning.status == ManualReview then Verifying else earning.status
        , payoutOrderId : earning.payoutOrderId
        , payoutOrderStatus: earning.payoutOrderStatus
        }
    )
    dailyEarnings

getOrderStatus :: PP.APIPaymentStatus -> OrderStatus
getOrderStatus status = 
  case status of
    PP.CHARGED -> CHARGED
    PP.AUTHORIZATION_FAILED -> FAILED
    PP.AUTHENTICATION_FAILED ->  FAILED
    PP.JUSPAY_DECLINED -> FAILED
    PP.NEW -> NEW
    PP.PENDING_VBV -> PENDING
    _ -> PENDING
