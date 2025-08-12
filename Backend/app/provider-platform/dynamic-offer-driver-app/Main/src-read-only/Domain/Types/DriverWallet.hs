{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DriverWallet where

import Data.Aeson
import qualified Data.Time
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Lib.Payment.Domain.Types.PayoutOrder
import qualified Tools.Beam.UtilsTH

data DriverWallet = DriverWallet
  { collectionAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    createdAt :: Data.Time.UTCTime,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Driver,
    driverPayable :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    gstDeduction :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    id :: Kernel.Types.Id.Id Domain.Types.DriverWallet.DriverWallet,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    merchantPayable :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    payoutOrderId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder),
    payoutStatus :: Kernel.Prelude.Maybe Domain.Types.DriverWallet.PayoutStatus,
    rideId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Ride.Ride),
    runningBalance :: Kernel.Types.Common.HighPrecMoney,
    transactionType :: Domain.Types.DriverWallet.TransactionType,
    updatedAt :: Data.Time.UTCTime
  }
  deriving (Generic, Show)

data PayoutStatus = INITIATED | FAILED | SETTLED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data TransactionType = RIDE_TRANSACTION | PAYOUT deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''PayoutStatus)

$(mkHttpInstancesForEnum ''PayoutStatus)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''TransactionType)

$(mkHttpInstancesForEnum ''TransactionType)
