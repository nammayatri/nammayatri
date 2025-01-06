{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PartnerOrgConfig (module Domain.Types.PartnerOrgConfig, module ReExport) where

import Data.Aeson
import Domain.Types.Extra.PartnerOrgConfig as ReExport
import qualified Domain.Types.Extra.PartnerOrgConfig
import qualified Domain.Types.PartnerOrganization
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data PartnerOrgConfig = PartnerOrgConfig
  { config :: Domain.Types.Extra.PartnerOrgConfig.PartnerOrganizationConfig,
    createdAt :: Kernel.Prelude.UTCTime,
    partnerOrgId :: Kernel.Types.Id.Id Domain.Types.PartnerOrganization.PartnerOrganization,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)
