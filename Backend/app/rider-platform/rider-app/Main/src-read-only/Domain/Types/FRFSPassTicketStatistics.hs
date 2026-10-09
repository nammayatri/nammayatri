{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FRFSPassTicketStatistics where

import Data.Aeson
import qualified Data.Time
import qualified Data.Time.Calendar
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.PurchasedPassPayment
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FRFSPassTicketStatistics = FRFSPassTicketStatistics
  { createdAt :: Data.Time.UTCTime,
    date :: Data.Time.Calendar.Day,
    fareAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    purchasedPassPaymentId :: Kernel.Types.Id.Id Domain.Types.PurchasedPassPayment.PurchasedPassPayment,
    savedAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    ticketCount :: Kernel.Prelude.Int,
    updatedAt :: Data.Time.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
