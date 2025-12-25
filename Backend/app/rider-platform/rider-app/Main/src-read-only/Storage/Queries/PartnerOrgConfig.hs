{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PartnerOrgConfig (module Storage.Queries.PartnerOrgConfig, module ReExport) where

import qualified Domain.Types.PartnerOrgConfig
import qualified Domain.Types.PartnerOrganization
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PartnerOrgConfig as Beam
import Storage.Queries.PartnerOrgConfigExtra as ReExport
import Storage.Queries.Transformers.PartnerOrgConfig

findAllByPartnerOrgId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.PartnerOrganization.PartnerOrganization -> m [Domain.Types.PartnerOrgConfig.PartnerOrgConfig])
findAllByPartnerOrgId partnerOrgId = do findAllWithKV [Se.Is Beam.partnerOrgId $ Se.Eq (Kernel.Types.Id.getId partnerOrgId)]
