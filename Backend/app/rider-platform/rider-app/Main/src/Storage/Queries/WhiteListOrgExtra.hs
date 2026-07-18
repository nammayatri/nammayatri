module Storage.Queries.WhiteListOrgExtra where

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

-- | Empty-vs-nonempty check for whitelist mode (call sites only use '== 0').
-- Fetches at most one row instead of loading the entire white_list_org table.
countTotalSubscribers :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => m Int
countTotalSubscribers =
  findAllWithOptionsKV
    [Se.Is Beam.id $ Se.Not $ Se.Eq ""]
    (Se.Asc Beam.id)
    (Just 1)
    (Just 0)
    <&> \rows -> if null rows then 0 else 1

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
findBySubscriberIdAndDomainAndMerchantId subscriberId domain merchantId = do listToMaybe <$> findAllWithKV [Se.And [Se.Is Beam.subscriberId $ Se.Eq (Kernel.Types.Id.getShortId subscriberId), Se.Is Beam.domain $ Se.Eq domain, Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)]]
