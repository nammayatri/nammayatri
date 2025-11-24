{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DispatcherHistory where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DispatcherHistory = DispatcherHistory
  { conductorCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    currentVehicle :: Kernel.Prelude.Text,
    depotId :: Kernel.Prelude.Text,
    dispatcherId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    driverCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.DispatcherHistory.DispatcherHistory,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    reasonContent :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    reasonTag :: Kernel.Prelude.Text,
    replacedVehicle :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    waybillNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
