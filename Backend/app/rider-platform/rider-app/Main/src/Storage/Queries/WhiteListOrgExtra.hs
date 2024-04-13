{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.WhiteListOrgExtra where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.WhiteListOrg as BeamBLO
import Storage.Queries.OrphanInstances.WhiteListOrg

-- Extra code goes here --

countTotalSubscribers :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => m Int
countTotalSubscribers = findAllWithKV [Se.Is BeamBLO.id $ Se.Not $ Se.Eq ""] <&> length

-- TODO:: remove it, For backward compatibility
findBySubscriberIdAndDomain :: (CacheFlow m r, EsqDBFlow m r) => ShortId Subscriber -> Domain -> m (Maybe WhiteListOrg)
findBySubscriberIdAndDomain subscriberId domain = do
  findOneWithKV
    [ Se.Is BeamBLO.subscriberId $ Se.Eq $ getShortId subscriberId,
      Se.Is BeamBLO.domain $ Se.Eq domain,
      Se.Is BeamBLO.merchantId $ Se.Eq ""
    ]