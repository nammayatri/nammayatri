{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.ParkingBooking where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.Person
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.PaymentOrder
import Servant
import Tools.Auth

data ParkingBookingReq = ParkingBookingReq
  { amount :: Kernel.Types.Common.HighPrecMoney,
    entityId :: Data.Text.Text,
    parkingEndTime :: Kernel.Prelude.UTCTime,
    parkingStartTime :: Kernel.Prelude.UTCTime,
    slotNumber :: Data.Text.Text,
    vehicleNumber :: Data.Text.Text,
    customerId :: Kernel.Types.Id.Id Domain.Types.Person.Person
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ParkingBookingResponse = ParkingBookingResponse {orderId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder, parkingLotId :: Data.Text.Text, paymentLink :: Data.Text.Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
