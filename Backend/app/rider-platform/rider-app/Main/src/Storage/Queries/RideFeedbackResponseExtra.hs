{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.RideFeedbackResponseExtra where

import qualified Domain.Types.Person as DP
import qualified Domain.Types.RideFeedbackResponse as DRFR
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.RideFeedbackResponse as Beam
import Storage.Queries.OrphanInstances.RideFeedbackResponse ()

-- | A rider's responses created at or after the given time, used for per-question cooldowns.
-- personId is a forced secondary key, so this is served from KV.
findAllByPersonIdSince :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DP.Person -> UTCTime -> m [DRFR.RideFeedbackResponse]
findAllByPersonIdSince personId since =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.personId $ Se.Eq personId.getId,
          Se.Is Beam.createdAt $ Se.GreaterThanOrEq since
        ]
    ]
