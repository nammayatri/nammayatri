{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MultiModalCalendar where

import Data.Aeson
import qualified Data.Time.Calendar
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data MultiModalCalendar = MultiModalCalendar
  { endDate :: Data.Time.Calendar.Day,
    friday :: Domain.Types.MultiModalCalendar.ServiceType,
    id :: Kernel.Types.Id.Id Domain.Types.MultiModalCalendar.MultiModalCalendar,
    monday :: Domain.Types.MultiModalCalendar.ServiceType,
    saturday :: Domain.Types.MultiModalCalendar.ServiceType,
    startDate :: Data.Time.Calendar.Day,
    sunday :: Domain.Types.MultiModalCalendar.ServiceType,
    thursday :: Domain.Types.MultiModalCalendar.ServiceType,
    tuesday :: Domain.Types.MultiModalCalendar.ServiceType,
    wednesday :: Domain.Types.MultiModalCalendar.ServiceType,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, (Show), (Eq))

data MultiModalDay = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data ServiceType = Service | NoService deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''MultiModalDay))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''ServiceType))
