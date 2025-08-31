{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.DriverWallet where

import Data.OpenApi (ToSchema)
import qualified Data.Time
import qualified Domain.Types.DriverWallet
import qualified Domain.Types.Location
import qualified Domain.Types.Ride
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data TopUpRequest = TopUpRequest {amount :: Kernel.Types.Common.HighPrecMoney}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TransactionDetails = TransactionDetails
  { collectionAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    createdAt :: Data.Time.UTCTime,
    driverPayable :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    fromLocation :: Kernel.Prelude.Maybe Domain.Types.Location.Location,
    gstDeduction :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    id :: Kernel.Types.Id.Id Domain.Types.DriverWallet.DriverWallet,
    merchantPayable :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    rideId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Ride.Ride),
    runningBalance :: Kernel.Types.Common.HighPrecMoney,
    toLocation :: Kernel.Prelude.Maybe Domain.Types.Location.Location,
    transactionType :: Domain.Types.DriverWallet.TransactionType
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TransactionResponse = TransactionResponse {currentBalance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney, transactions :: [TransactionDetails]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
