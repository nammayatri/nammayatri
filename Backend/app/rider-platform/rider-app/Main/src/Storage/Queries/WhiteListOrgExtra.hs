{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.WhiteListOrgExtra where

import Domain.Types.Merchant
import Domain.Types.WhiteListOrg
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Beckn.Domain (Domain (..))
import Kernel.Types.Id
import Kernel.Types.Registry.Subscriber (Subscriber)
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.WhiteListOrg as BeamBLO
import Storage.Queries.OrphanInstances.WhiteListOrg

-- Extra code goes here --

countTotalSubscribers :: KvDbFlow m r => m Int
countTotalSubscribers = findAllWithKV [Se.Is BeamBLO.id $ Se.Not $ Se.Eq ""] <&> length

-- TODO:: remove it, For backward compatibility
findBySubscriberIdAndDomain :: KvDbFlow m r => ShortId Subscriber -> Domain -> m (Maybe WhiteListOrg)
findBySubscriberIdAndDomain subscriberId domain = do
  findOneWithKV
    [ Se.Is BeamBLO.subscriberId $ Se.Eq $ getShortId subscriberId,
      Se.Is BeamBLO.domain $ Se.Eq domain,
      Se.Is BeamBLO.merchantId $ Se.Eq ""
    ]
