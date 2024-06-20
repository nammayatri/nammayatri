{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.ClientPersonInfo where

import Data.Aeson
import qualified Domain.Types.BecknConfig
import qualified Domain.Types.Client
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data ClientPersonInfo = ClientPersonInfo
  { id :: Kernel.Types.Id.Id Domain.Types.ClientPersonInfo.ClientPersonInfo,
    clientId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Client.Client),
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.BecknConfig.VehicleCategory,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    rideCount :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
