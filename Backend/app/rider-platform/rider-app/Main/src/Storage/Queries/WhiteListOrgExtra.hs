module Storage.Queries.WhiteListOrgExtra where

import Control.Lens ((^?), _head)
import qualified Domain.Types.Merchant
import Domain.Types.WhiteListOrg
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Beckn.Domain (Domain (..))
import Kernel.Types.Id
import Kernel.Types.Registry.Subscriber (Subscriber)
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.WhiteListOrg as Beam
import Storage.Queries.OrphanInstances.WhiteListOrg ()

-- Extra code goes here --

countTotalSubscribers :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => m Int
countTotalSubscribers = findAllWithKV [Se.Is Beam.id $ Se.Not $ Se.Eq ""] <&> length

-- TODO:: remove it, For backward compatibility
findBySubscriberIdAndDomain :: (CacheFlow m r, EsqDBFlow m r) => ShortId Subscriber -> Domain -> m (Maybe WhiteListOrg)
findBySubscriberIdAndDomain subscriberId domain = do
  findOneWithKV
    [ Se.Is Beam.subscriberId $ Se.Eq $ getShortId subscriberId,
      Se.Is Beam.domain $ Se.Eq domain,
      Se.Is Beam.merchantId $ Se.Eq ""
    ]

-- TODO:: remove it, For backward compatibility
findBySubscriberIdAndDomainAndMerchantId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (ShortId Subscriber -> Domain -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m (Maybe Domain.Types.WhiteListOrg.WhiteListOrg))
findBySubscriberIdAndDomainAndMerchantId subscriberId domain merchantId = do (^? _head) <$> findAllWithKV [Se.And [Se.Is Beam.subscriberId $ Se.Eq (Kernel.Types.Id.getShortId subscriberId), Se.Is Beam.domain $ Se.Eq domain, Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)]]
