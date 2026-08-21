{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.VehicleActionHistory where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data VehicleActionHistory = VehicleActionHistory
  { action :: Domain.Types.VehicleActionHistory.VehicleActionType,
    conductorCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    currentVehicle :: Kernel.Prelude.Text,
    depotId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    dispatcherId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    driverCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.VehicleActionHistory.VehicleActionHistory,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    reasonContent :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    reasonTag :: Kernel.Prelude.Text,
    replacedVehicle :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    waybillNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data VehicleActionType = DISPATCHER | BLOCKER deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema), (ToParamSchema))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''VehicleActionType))
