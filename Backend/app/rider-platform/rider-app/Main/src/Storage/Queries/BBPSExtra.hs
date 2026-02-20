{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BBPSExtra where

import Domain.Types.BBPS
import Domain.Types.Merchant
import Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import Sequelize as Se
import qualified Storage.Beam.BBPS as BeamT
import Storage.Queries.OrphanInstances.BBPS

-- Extra code goes here --

getAllBBPSOrders :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Int -> Maybe Int -> Maybe Bool -> Maybe Domain.Types.BBPS.BBPSPaymentStatus -> Id Person -> Id Merchant -> m [Domain.Types.BBPS.BBPS]
getAllBBPSOrders limit offset mbActive bbpsStatus personId merchantId =
  findAllWithOptionsKV
    ( [ Se.And
          [ Se.Is BeamT.customerId $ Se.Eq personId.getId,
            Se.Is BeamT.merchantId $ Se.Eq merchantId.getId
          ]
      ]
        <> maybe activeConditionCheck (\s -> [Se.Is BeamT.status $ Se.Eq s]) bbpsStatus
    )
    (Se.Desc BeamT.createdAt)
    (Just limit)
    offset
  where
    activeConditionCheck =
      maybe
        []
        ( \case
            True -> [Se.Is BeamT.status $ Se.Not $ Se.In notActiveStates]
            False -> [Se.Is BeamT.status $ Se.In notActiveStates]
        )
        mbActive
    notActiveStates = [REFUNDED, FAILED, SUCCESS]
