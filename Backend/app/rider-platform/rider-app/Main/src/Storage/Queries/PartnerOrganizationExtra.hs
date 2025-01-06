module Storage.Queries.PartnerOrganizationExtra where

import Domain.Types.PartnerOrganization
import Kernel.Beam.Functions
import Kernel.External.Encryption (DbHash)
import Kernel.Prelude
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.PartnerOrganization as Beam
import Storage.Queries.OrphanInstances.PartnerOrganization ()

-- Extra code goes here --

findByApiKeyHash :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DbHash -> m (Maybe PartnerOrganization)
findByApiKeyHash apiKeyHash = findOneWithKV [Se.Is Beam.apiKeyHash $ Se.Eq apiKeyHash]
