{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.CorporateRoute where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data CorporateRoute = CorporateRoute
  { id :: Kernel.Types.Id.Id Domain.Types.CorporateRoute.CorporateRoute,
    shiftId :: Kernel.Prelude.Text,
    routeCode :: Kernel.Prelude.Text,
    direction :: CorporateRouteDirection,
    estimatedDurationMinutes :: Kernel.Prelude.Int,
    estimatedDistanceMeters :: Kernel.Prelude.Int,
    vehicleTier :: Kernel.Prelude.Text,
    maxCapacity :: Kernel.Prelude.Int,
    status :: CorporateRouteStatus,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data CorporateRouteDirection = PICKUP | DROP
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data CorporateRouteStatus = CR_ACTIVE | CR_INACTIVE | OPTIMIZING | CR_ARCHIVED
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''CorporateRouteDirection)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''CorporateRouteStatus)
