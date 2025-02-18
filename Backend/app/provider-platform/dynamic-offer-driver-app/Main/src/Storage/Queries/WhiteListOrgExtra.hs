module Storage.Queries.WhiteListOrgExtra where

import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.WhiteListOrg
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Domain
import Kernel.Types.Id
import qualified Kernel.Types.Registry
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.WhiteListOrg as Beam
import Storage.Queries.OrphanInstances.WhiteListOrg ()

-- Extra code goes here --

countTotalSubscribers :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Merchant -> Id MerchantOperatingCity -> m Int
countTotalSubscribers merchantId merchantOperatingCityId =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)
        ]
    ]
    <&> length

findBySubscriberIdAndDomain ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.ShortId Kernel.Types.Registry.Subscriber -> Kernel.Types.Beckn.Domain.Domain -> m (Maybe Domain.Types.WhiteListOrg.WhiteListOrg))
findBySubscriberIdAndDomain subscriberId domain = do listToMaybe <$> findAllWithKV [Se.And [Se.Is Beam.subscriberId $ Se.Eq (Kernel.Types.Id.getShortId subscriberId), Se.Is Beam.domain $ Se.Eq domain]]
