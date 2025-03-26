{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.VehicleInfo where

import Data.Aeson
import qualified Domain.Types.VehicleRegistrationCertificate
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data VehicleInfo = VehicleInfo
  { answer :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.VehicleInfo.VehicleInfo,
    question :: Kernel.Prelude.Text,
    questionName :: Kernel.Prelude.Text,
    rcId :: Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
