{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.TicketService where

import Data.Aeson
import qualified Data.Time
import qualified Domain.Types.BusinessHour
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified SharedLogic.TicketRule.Core
import qualified Tools.Beam.UtilsTH

data TicketService = TicketService
  { allowCancellation :: Kernel.Prelude.Bool,
    allowFutureBooking :: Kernel.Prelude.Bool,
    businessHours :: [Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour],
    expiry :: Domain.Types.TicketService.ExpiryType,
    id :: Kernel.Types.Id.Id Domain.Types.TicketService.TicketService,
    isClosed :: Kernel.Prelude.Bool,
    maxVerification :: Kernel.Prelude.Int,
    operationalDate :: Kernel.Prelude.Maybe Domain.Types.TicketService.OperationalDate,
    operationalDays :: [Kernel.Prelude.Text],
    placesId :: Kernel.Prelude.Text,
    rules :: Kernel.Prelude.Maybe [SharedLogic.TicketRule.Core.Rule],
    service :: Kernel.Prelude.Text,
    shortDesc :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data ExpiryType = InstantExpiry Kernel.Prelude.Int | VisitDate Kernel.Prelude.TimeOfDay deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data OperationalDate = OperationalDate {eneDate :: Data.Time.Day, startDate :: Data.Time.Day} deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq, Ord)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ExpiryType)
