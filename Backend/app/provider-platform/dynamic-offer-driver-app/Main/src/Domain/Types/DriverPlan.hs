{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.DriverPlan where

import qualified Domain.Types.Mandate as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Plan as DPlan
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id

data DriverPlan = DriverPlan
  { driverId :: Id DP.Person,
    planId :: Id DPlan.Plan,
    planType :: DPlan.PaymentMode,
    mandateId :: Maybe (Id DM.Mandate),
    mandateSetupDate :: Maybe UTCTime,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    coinCovertedToCashLeft :: HighPrecMoney,
    totalCoinsConvertedCash :: HighPrecMoney
  }
  deriving (Generic, Show)
