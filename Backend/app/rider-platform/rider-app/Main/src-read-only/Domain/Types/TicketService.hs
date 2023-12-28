{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.TicketService where

import qualified Domain.Types.BusinessHour
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import Tools.Beam.UtilsTH

data TicketService = TicketService
  { allowFutureBooking :: Kernel.Prelude.Bool,
    businessHours :: [Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour],
    expiry :: Domain.Types.TicketService.ExpiryType,
    id :: Kernel.Types.Id.Id Domain.Types.TicketService.TicketService,
    maxVerification :: Kernel.Prelude.Int,
    operationalDays :: [Kernel.Prelude.Text],
    placesId :: Kernel.Prelude.Text,
    service :: Kernel.Prelude.Text,
    shortDesc :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data ExpiryType = InstantExpiry Kernel.Prelude.Int | VisitDate Kernel.Prelude.TimeOfDay
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnum ''ExpiryType)
