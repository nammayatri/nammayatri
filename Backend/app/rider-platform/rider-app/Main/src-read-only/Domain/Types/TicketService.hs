{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.TicketService where

import qualified Domain.Types.BusinessHour as Domain.Types.BusinessHour
import Kernel.Prelude
import qualified Kernel.Types.Id as Kernel.Types.Id
import Tools.Beam.UtilsTH

data TicketService = TicketService
  { allowFutureBooking :: Kernel.Prelude.Bool,
    businessHours :: [Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour],
    expiry :: Domain.Types.TicketService.ExpiryType,
    id :: Kernel.Types.Id.Id Domain.Types.TicketService.TicketService,
    maxVerification :: Kernel.Prelude.Int,
    operationalDays :: [Kernel.Prelude.Text],
    placeId :: Kernel.Prelude.Text,
    serviceName :: Kernel.Prelude.Text,
    shortDesc :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, Show)

data ExpiryType = SameDay | NextDay | NextWeek | NextMonth
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnum ''ExpiryType)
