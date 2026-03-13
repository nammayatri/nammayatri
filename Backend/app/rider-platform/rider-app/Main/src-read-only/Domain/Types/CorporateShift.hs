{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.CorporateShift (module Domain.Types.CorporateShift, module ReExport) where

import Data.Aeson
import Data.Time (TimeOfDay)
import Domain.Types.Extra.CorporateShift as ReExport
import qualified Domain.Types.CorporateEntity
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data CorporateShift = CorporateShift
  { id :: Kernel.Types.Id.Id Domain.Types.CorporateShift.CorporateShift,
    corporateEntityId :: Kernel.Types.Id.Id Domain.Types.CorporateEntity.CorporateEntity,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    name :: Kernel.Prelude.Text,
    pickupWindowStart :: Data.Time.TimeOfDay,
    pickupWindowEnd :: Data.Time.TimeOfDay,
    dropWindowStart :: Data.Time.TimeOfDay,
    dropWindowEnd :: Data.Time.TimeOfDay,
    activeDays :: Kernel.Prelude.Text,
    isNightShift :: Kernel.Prelude.Bool,
    maxOccupancy :: Kernel.Prelude.Int,
    allowedVehicleTiers :: Kernel.Prelude.Text,
    confirmationDeadlineMinutes :: Kernel.Prelude.Int,
    status :: Domain.Types.CorporateShift.CorporateShiftStatus,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data CorporateShiftStatus = CS_ACTIVE | CS_INACTIVE | CS_ARCHIVED
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''CorporateShiftStatus)
