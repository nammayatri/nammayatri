{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PTCircuitBreakerHistory where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data PTCircuitBreakerHistory = PTCircuitBreakerHistory
  { apiType :: Domain.Types.PTCircuitBreakerHistory.APIType,
    createdAt :: Kernel.Prelude.UTCTime,
    failureCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    id :: Kernel.Types.Id.Id Domain.Types.PTCircuitBreakerHistory.PTCircuitBreakerHistory,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    newState :: Domain.Types.PTCircuitBreakerHistory.CircuitState,
    previousState :: Domain.Types.PTCircuitBreakerHistory.CircuitState,
    ptMode :: Domain.Types.PTCircuitBreakerHistory.PTMode,
    reason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data APIType = FareAPI | BookingAPI deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data CircuitState = Open | Closed | HalfOpen deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data PTMode = Metro | Bus | Subway deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''APIType))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''CircuitState))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''PTMode))
