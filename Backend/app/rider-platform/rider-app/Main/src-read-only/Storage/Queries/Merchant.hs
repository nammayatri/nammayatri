{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Merchant (module Storage.Queries.Merchant, module ReExport) where

import qualified Domain.Types.Merchant
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import qualified Kernel.Types.Registry
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant as Beam
import Storage.Queries.MerchantExtra as ReExport
import Storage.Queries.Transformers.Merchant

create :: KvDbFlow m r => (Domain.Types.Merchant.Merchant -> m ())
create = createWithKV

findById :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m (Maybe Domain.Types.Merchant.Merchant))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]

findByShortId :: KvDbFlow m r => (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> m (Maybe Domain.Types.Merchant.Merchant))
findByShortId (Kernel.Types.Id.ShortId shortId) = do findOneWithKV [Se.Is Beam.shortId $ Se.Eq shortId]

findBySubscriberId :: KvDbFlow m r => (Kernel.Types.Id.ShortId Kernel.Types.Registry.Subscriber -> m (Maybe Domain.Types.Merchant.Merchant))
findBySubscriberId (Kernel.Types.Id.ShortId subscriberId) = do findOneWithKV [Se.Is Beam.subscriberId $ Se.Eq subscriberId]
