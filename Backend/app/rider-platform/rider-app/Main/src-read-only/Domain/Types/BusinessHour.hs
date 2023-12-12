{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.BusinessHour where

import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.ServiceCategory
import Kernel.Prelude
import qualified Kernel.Types.Id
import Tools.Beam.UtilsTH

data BusinessHour = BusinessHour
  { btype :: Domain.Types.BusinessHour.BusinessHourType,
    categoryId :: [Kernel.Types.Id.Id Domain.Types.ServiceCategory.ServiceCategory],
    id :: Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data BusinessHourType = Slot Kernel.Prelude.TimeOfDay | Duration Kernel.Prelude.TimeOfDay Kernel.Prelude.TimeOfDay
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnum ''BusinessHourType)
