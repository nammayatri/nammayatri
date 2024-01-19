{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.Station where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import Tools.Beam.UtilsTH

data Station = Station
  { address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    code :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.Station.Station,
    lat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    name :: Kernel.Prelude.Text,
    vehicleType :: Domain.Types.Station.FRFSVehicleType,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FRFSVehicleType = METRO
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkBeamInstancesForEnum ''FRFSVehicleType)

$(mkHttpInstancesForEnum ''FRFSVehicleType)
