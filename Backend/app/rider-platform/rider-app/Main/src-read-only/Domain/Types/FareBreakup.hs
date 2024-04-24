{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FareBreakup where

import Data.Aeson
import qualified Domain.Types.Booking
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FareBreakup = FareBreakup
  { amount :: Kernel.Types.Common.Price,
    bookingId :: Kernel.Types.Id.Id Domain.Types.Booking.Booking,
    description :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.FareBreakup.FareBreakup
  }
  deriving (Show)

data FareBreakupAPIEntity = FareBreakupAPIEntity {amount :: Kernel.Types.Common.HighPrecMoney, amountWithCurrency :: Kernel.Types.Common.PriceAPIEntity, description :: Kernel.Prelude.Text}
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
