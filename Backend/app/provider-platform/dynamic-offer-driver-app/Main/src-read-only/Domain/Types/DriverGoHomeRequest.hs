{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DriverGoHomeRequest where

import Data.Aeson
import qualified Domain.Types.Person
import qualified Kernel.External.Maps.HasCoordinates
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DriverGoHomeRequest = DriverGoHomeRequest
  { createdAt :: Kernel.Prelude.UTCTime,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Kernel.Types.Id.Id Domain.Types.DriverGoHomeRequest.DriverGoHomeRequest,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    mbReachedHome :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    numCancellation :: Kernel.Prelude.Int,
    status :: Domain.Types.DriverGoHomeRequest.DriverGoHomeRequestStatus,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Kernel.External.Maps.HasCoordinates.HasCoordinates, Show)

data DriverGoHomeRequestStatus = ACTIVE | SUCCESS | FAILED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''DriverGoHomeRequestStatus)

{-
	DSL Source Link: file://./../../../spec/Storage/DriverGoHome.yaml
-}
