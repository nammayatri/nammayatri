{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.FRFSSearch where

import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Station
import Kernel.Prelude
import qualified Kernel.Types.Id

data FRFSSearch = FRFSSearch
  { from :: Kernel.Types.Id.Id Domain.Types.Station.Station,
    id :: Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch,
    quantity :: Kernel.Prelude.Int,
    to :: Kernel.Types.Id.Id Domain.Types.Station.Station,
    vehicleType :: Domain.Types.Station.FRFSVehicleType,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
