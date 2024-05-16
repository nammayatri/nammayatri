{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.WhiteListOrg (module Storage.Queries.WhiteListOrg, module ReExport) where

import qualified Domain.Types.Merchant
import qualified Domain.Types.WhiteListOrg
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Domain
import Kernel.Types.Error
import qualified Kernel.Types.Id
import qualified Kernel.Types.Registry
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.WhiteListOrg as Beam
import Storage.Queries.Transformers.WhiteListOrg
import Storage.Queries.WhiteListOrgExtra as ReExport

create :: KvDbFlow m r => (Domain.Types.WhiteListOrg.WhiteListOrg -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.WhiteListOrg.WhiteListOrg] -> m ())
createMany = traverse_ create

findBySubscriberIdAndDomainAndMerchantId ::
  KvDbFlow m r =>
  (Kernel.Types.Id.ShortId Kernel.Types.Registry.Subscriber -> Kernel.Types.Beckn.Domain.Domain -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m (Maybe Domain.Types.WhiteListOrg.WhiteListOrg))
findBySubscriberIdAndDomainAndMerchantId (Kernel.Types.Id.ShortId subscriberId) domain (Kernel.Types.Id.Id merchantId) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.subscriberId $ Se.Eq subscriberId,
          Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.merchantId $ Se.Eq merchantId
        ]
    ]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.WhiteListOrg.WhiteListOrg -> m (Maybe Domain.Types.WhiteListOrg.WhiteListOrg))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.WhiteListOrg.WhiteListOrg -> m ())
updateByPrimaryKey (Domain.Types.WhiteListOrg.WhiteListOrg {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.createdAt (Kernel.Prelude.Just createdAt),
      Se.Set Beam.domain domain,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.subscriberId (Kernel.Types.Id.getShortId subscriberId),
      Se.Set Beam.updatedAt (Just _now)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
