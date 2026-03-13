{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.CorporateRouteStopAssignment where

import Data.Aeson
import qualified Domain.Types.CorporateEmployee
import qualified Domain.Types.CorporateRouteStop
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data CorporateRouteStopAssignment = CorporateRouteStopAssignment
  { id :: Kernel.Types.Id.Id Domain.Types.CorporateRouteStopAssignment.CorporateRouteStopAssignment,
    routeStopId :: Kernel.Types.Id.Id Domain.Types.CorporateRouteStop.CorporateRouteStop,
    employeeId :: Kernel.Types.Id.Id Domain.Types.CorporateEmployee.CorporateEmployee,
    effectiveFrom :: Kernel.Prelude.UTCTime,
    effectiveTo :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)
