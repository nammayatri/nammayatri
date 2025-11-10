{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PurchasedPassPaymentExtra where

import qualified Data.Time.Calendar
import qualified Domain.Types.PurchasedPass as DPurchasedPass
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PurchasedPassPayment as Beam
import Storage.Queries.OrphanInstances.PurchasedPassPayment

expireAllActiveOrPreBookedPassPaymentsWithinGivenDateRange ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id DPurchasedPass.PurchasedPass -> Data.Time.Calendar.Day -> Data.Time.Calendar.Day -> m ())
expireAllActiveOrPreBookedPassPaymentsWithinGivenDateRange purchasedPassId startDate endDate = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.status DPurchasedPass.Expired, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.purchasedPassId $ Se.Eq (Kernel.Types.Id.getId purchasedPassId),
          Se.Is Beam.startDate $ Se.GreaterThanOrEq startDate,
          Se.Is Beam.endDate $ Se.LessThanOrEq endDate,
          Se.Is Beam.status $ Se.In [DPurchasedPass.Active, DPurchasedPass.PreBooked]
        ]
    ]
