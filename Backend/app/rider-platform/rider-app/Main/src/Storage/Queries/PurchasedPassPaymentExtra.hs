{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PurchasedPassPaymentExtra where

import Data.Time hiding (getCurrentTime)
import qualified Domain.Types.PurchasedPass as DPurchasedPass
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PurchasedPassPayment as Beam
import Storage.Queries.OrphanInstances.PurchasedPassPayment

expireOlderActivePaymentsByPurchasedPassId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DPurchasedPass.PurchasedPass ->
  Day ->
  m ()
expireOlderActivePaymentsByPurchasedPassId purchasedPassId endDate = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.status DPurchasedPass.Expired, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.purchasedPassId $ Se.Eq purchasedPassId.getId,
          Se.Is Beam.status $ Se.Eq DPurchasedPass.Active,
          Se.Is Beam.endDate $ Se.LessThanOrEq endDate
        ]
    ]
