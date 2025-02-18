{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PartnerOrgStation where

import Data.Aeson
import qualified Domain.Types.PartnerOrganization
import qualified Domain.Types.Station
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data PartnerOrgStation = PartnerOrgStation
  { name :: Kernel.Prelude.Text,
    partnerOrgId :: Kernel.Types.Id.Id Domain.Types.PartnerOrganization.PartnerOrganization,
    partnerOrgStationId :: Kernel.Types.Id.Id Domain.Types.PartnerOrgStation.PartnerOrgStation,
    stationId :: Kernel.Types.Id.Id Domain.Types.Station.Station,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)
