{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.CorporateRouteStop where

import Data.Aeson
import qualified Domain.Types.CorporateRoute
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data CorporateRouteStop = CorporateRouteStop
  { id :: Kernel.Types.Id.Id Domain.Types.CorporateRouteStop.CorporateRouteStop,
    routeId :: Kernel.Types.Id.Id Domain.Types.CorporateRoute.CorporateRoute,
    sequence :: Kernel.Prelude.Int,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    address :: Kernel.Prelude.Text,
    estimatedArrivalOffset :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)
