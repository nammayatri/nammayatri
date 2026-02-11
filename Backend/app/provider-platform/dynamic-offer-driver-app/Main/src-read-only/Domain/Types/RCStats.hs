{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RCStats where

import Data.Aeson
import qualified Domain.Types.VehicleRegistrationCertificate
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data RCStats = RCStats {rcId :: Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate, totalRides :: Kernel.Prelude.Int, updatedAt :: Kernel.Prelude.UTCTime}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
