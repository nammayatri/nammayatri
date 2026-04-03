{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.PartnerOrgStation where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.PartnerOrganization
import qualified Tools.Beam.UtilsTH



data PartnerOrgStation
    = PartnerOrgStation {name :: Kernel.Prelude.Text,
                         partnerOrgId :: Kernel.Types.Id.Id Domain.Types.PartnerOrganization.PartnerOrganization,
                         partnerOrgStationId :: Kernel.Types.Id.Id Domain.Types.PartnerOrgStation.PartnerOrgStation,
                         stationCode :: Kernel.Prelude.Text,
                         createdAt :: Kernel.Prelude.UTCTime,
                         updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, ( Show), ( ToJSON), ( FromJSON))



