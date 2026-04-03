{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.PartnerInvoiceDataLog where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Booking
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Tools.Beam.UtilsTH



data PartnerInvoiceDataLog
    = PartnerInvoiceDataLog {bookingId :: Kernel.Types.Id.Id Domain.Types.Booking.Booking,
                             createdAt :: Kernel.Prelude.UTCTime,
                             exportedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
                             id :: Kernel.Types.Id.Id Domain.Types.PartnerInvoiceDataLog.PartnerInvoiceDataLog,
                             merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                             merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                             personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
                             requestedAt :: Kernel.Prelude.UTCTime,
                             updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, ( Show), ( Eq))



