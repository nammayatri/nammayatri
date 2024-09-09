{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MultiModalNetwork where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data MultiModalNetwork = MultiModalNetwork
  { id :: Kernel.Types.Id.Id Domain.Types.MultiModalNetwork.MultiModalNetwork,
    name :: Kernel.Prelude.Text,
    networkClass :: Kernel.Prelude.Text,
    networkCode :: Kernel.Prelude.Text,
    networkType :: Domain.Types.MultiModalNetwork.MultiModalNetworkType,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, (Show), (Eq))

data MultiModalNetworkType = Bus | Train | Metro | Taxi deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''MultiModalNetworkType))
