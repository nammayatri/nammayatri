{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.TicketSubPlace where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.TicketPlace
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified SharedLogic.TicketRule.Core
import qualified Tools.Beam.UtilsTH

data TicketSubPlace = TicketSubPlace
  { createdAt :: Kernel.Prelude.UTCTime,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    enforcedTicketPlaceId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace),
    id :: Kernel.Types.Id.Id Domain.Types.TicketSubPlace.TicketSubPlace,
    isActive :: Kernel.Prelude.Bool,
    name :: Kernel.Prelude.Text,
    rules :: Kernel.Prelude.Maybe [SharedLogic.TicketRule.Core.Rule],
    subPlaceType :: Domain.Types.TicketSubPlace.SubPlaceType,
    ticketPlaceId :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace,
    updatedAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data SubPlaceType = Venue | Terminal | Dock | Screen | Hall | Platform deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''SubPlaceType)
