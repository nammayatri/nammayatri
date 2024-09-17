{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MultiModalTimeFrame where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MultiModalCalendar
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data MultiModalTimeFrame = MultiModalTimeFrame
  { endTime :: Kernel.Prelude.TimeOfDay,
    id :: Kernel.Types.Id.Id Domain.Types.MultiModalTimeFrame.MultiModalTimeFrame,
    serviceId :: Kernel.Types.Id.Id Domain.Types.MultiModalCalendar.MultiModalCalendar,
    startTime :: Kernel.Prelude.TimeOfDay,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, (Show), (Eq))
