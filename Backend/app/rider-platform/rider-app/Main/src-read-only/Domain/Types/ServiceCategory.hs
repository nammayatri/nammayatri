{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.ServiceCategory where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.ServicePeopleCategory
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified SharedLogic.TicketRule.Core
import qualified Tools.Beam.UtilsTH

data ServiceCategory = ServiceCategory
  { allowedSeats :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    availableSeats :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    description :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory,
    inclusionPoints :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    isClosed :: Kernel.Prelude.Bool,
    maxSelection :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    name :: Kernel.Prelude.Text,
    peopleCategory :: [Kernel.Types.Id.Id Domain.Types.ServicePeopleCategory.ServicePeopleCategory],
    placeId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    remainingActions :: Kernel.Prelude.Maybe [SharedLogic.TicketRule.Core.ActionType],
    rules :: Kernel.Prelude.Maybe [SharedLogic.TicketRule.Core.Rule],
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
