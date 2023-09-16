{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.DueDetailsList.Controller where

import Screens.Types (PromoConfig)

data Action = SelectDue Int

type DueDetailsListState = {
  dues :: Array DueCard
}

type DueCard = {
  date :: String,
  planType :: String,
  offerApplied :: PromoConfig,
  noOfRides :: String,
  totalEarningsOfDay :: String,
  dueAmount :: String,
  fareBreakup :: String,
  expanded :: Boolean,
  isAutoPayFailed :: Boolean,
  isSplitPayment :: Boolean
}