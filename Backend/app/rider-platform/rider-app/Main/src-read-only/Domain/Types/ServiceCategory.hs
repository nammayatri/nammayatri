{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.ServiceCategory where

import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.ServicePeopleCategory
import Kernel.Prelude
import qualified Kernel.Types.Id

data ServiceCategory = ServiceCategory
  { allowedSeats :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    availableSeats :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    description :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory,
    name :: Kernel.Prelude.Text,
    peopleCategory :: [Kernel.Types.Id.Id Domain.Types.ServicePeopleCategory.ServicePeopleCategory],
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
