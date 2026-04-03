{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.PartnerOrgConfig (module Storage.Queries.PartnerOrgConfig, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.PartnerOrgConfigExtra as ReExport
import Storage.Queries.Transformers.PartnerOrgConfig
import qualified Domain.Types.PartnerOrgConfig
import qualified Storage.Beam.PartnerOrgConfig as Beam
import qualified Kernel.Types.Id
import qualified Domain.Types.PartnerOrganization
import qualified Sequelize as Se



findAllByPartnerOrgId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                         (Kernel.Types.Id.Id Domain.Types.PartnerOrganization.PartnerOrganization -> m ([Domain.Types.PartnerOrgConfig.PartnerOrgConfig]))
findAllByPartnerOrgId partnerOrgId = do findAllWithKV [Se.Is Beam.partnerOrgId $ Se.Eq (Kernel.Types.Id.getId partnerOrgId)]



