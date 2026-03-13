{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.CorporateRoute (module Domain.Types.CorporateRoute, module ReExport) where

import Data.Aeson
import Domain.Types.Extra.CorporateRoute as ReExport
import qualified Domain.Types.CorporateEntity
import qualified Domain.Types.CorporateShift
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data CorporateRoute = CorporateRoute
  { id :: Kernel.Types.Id.Id Domain.Types.CorporateRoute.CorporateRoute,
    corporateEntityId :: Kernel.Types.Id.Id Domain.Types.CorporateEntity.CorporateEntity,
    shiftId :: Kernel.Types.Id.Id Domain.Types.CorporateShift.CorporateShift,
    routeCode :: Kernel.Prelude.Text,
    direction :: Domain.Types.CorporateRoute.CorporateRouteDirection,
    estimatedDurationMinutes :: Kernel.Prelude.Int,
    estimatedDistanceMeters :: Kernel.Prelude.Int,
    vehicleTier :: Kernel.Prelude.Text,
    maxCapacity :: Kernel.Prelude.Int,
    polyline :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Domain.Types.CorporateRoute.CorporateRouteStatus,
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
