{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SpecialOccasion where

import Data.Aeson
import qualified Data.Time.Calendar
import qualified Domain.Types.BusinessHour
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data SpecialOccasion = SpecialOccasion
  { id :: Kernel.Types.Id.Id Domain.Types.SpecialOccasion.SpecialOccasion,
    entityId :: Kernel.Prelude.Text,
    date :: Kernel.Prelude.Maybe Data.Time.Calendar.Day,
    dayOfWeek :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    specialDayType :: Domain.Types.SpecialOccasion.SpecialDayType,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    businessHours :: [Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour],
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data SpecialDayType = Open | Closed deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''SpecialDayType)
