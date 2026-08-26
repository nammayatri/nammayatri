{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PurchasedPassPaymentExtra where

import Data.Time hiding (getCurrentTime)
import qualified Domain.Types.PurchasedPass as DPurchasedPass
import qualified Domain.Types.PurchasedPassPayment as DPurchasedPassPayment
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Sequelize as Se
import qualified Storage.Beam.PurchasedPassPayment as Beam
import Storage.Queries.OrphanInstances.PurchasedPassPayment

expireOlderPaymentsByPurchasedPassId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DPurchasedPass.PurchasedPass ->
  Day ->
  m ()
expireOlderPaymentsByPurchasedPassId purchasedPassId endDate = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.status DPurchasedPass.Expired, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.purchasedPassId $ Se.Eq purchasedPassId.getId,
          Se.Is Beam.status $ Se.In [DPurchasedPass.Active, DPurchasedPass.PreBooked],
          Se.Is Beam.endDate $ Se.LessThan endDate
        ]
    ]

expireOlderPaymentsByPurchasedPassIds ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  [Id DPurchasedPass.PurchasedPass] ->
  Day ->
  m ()
expireOlderPaymentsByPurchasedPassIds [] _ = pure ()
expireOlderPaymentsByPurchasedPassIds purchasedPassIds endDate = do
  now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.status DPurchasedPass.Expired, Se.Set Beam.updatedAt now]
    [ Se.And
        [ Se.Is Beam.purchasedPassId $ Se.In (map getId purchasedPassIds),
          Se.Is Beam.status $ Se.In [DPurchasedPass.Active, DPurchasedPass.PreBooked],
          Se.Is Beam.endDate $ Se.LessThan endDate
        ]
    ]

updateStatusToPhotoPendingByPurchasedPassIds ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  [Id DPurchasedPass.PurchasedPass] ->
  [Text] ->
  Day ->
  m ()
updateStatusToPhotoPendingByPurchasedPassIds [] _ _ = pure ()
updateStatusToPhotoPendingByPurchasedPassIds _ [] _ = pure ()
updateStatusToPhotoPendingByPurchasedPassIds purchasedPassIds photoPassCodes today = do
  now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.status DPurchasedPass.PhotoPending, Se.Set Beam.updatedAt now]
    [ Se.And
        [ Se.Is Beam.purchasedPassId $ Se.In (map getId purchasedPassIds),
          Se.Is Beam.passCode $ Se.In photoPassCodes,
          Se.Is Beam.status $ Se.In [DPurchasedPass.PreBooked, DPurchasedPass.Active],
          Se.Is Beam.startDate $ Se.LessThanOrEq today,
          Se.Is Beam.endDate $ Se.GreaterThanOrEq today,
          Se.Is Beam.passPhotoMediaId $ Se.Eq Nothing,
          Se.Is Beam.profilePicture $ Se.Eq Nothing
        ]
    ]

activatePreBookedPaymentsByPurchasedPassIds ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  [Id DPurchasedPass.PurchasedPass] ->
  Day ->
  m ()
activatePreBookedPaymentsByPurchasedPassIds [] _ = pure ()
activatePreBookedPaymentsByPurchasedPassIds purchasedPassIds today = do
  now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.status DPurchasedPass.Active, Se.Set Beam.updatedAt now, Se.Set Beam.activatedAt (Just now)]
    [ Se.And
        [ Se.Is Beam.purchasedPassId $ Se.In (map getId purchasedPassIds),
          Se.Is Beam.status $ Se.Eq DPurchasedPass.PreBooked,
          Se.Is Beam.endDate $ Se.GreaterThanOrEq today
        ]
    ]

updatePurchaseDataByPurchasedPassIdAndStartEndDate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DPurchasedPass.PurchasedPass ->
  Day ->
  Day ->
  Day ->
  Day ->
  DPurchasedPass.StatusType ->
  m ()
updatePurchaseDataByPurchasedPassIdAndStartEndDate purchasedPassId oldStartDate oldEndDate newStartDate newEndDate status = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.startDate newStartDate, Se.Set Beam.endDate newEndDate, Se.Set Beam.status status, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.purchasedPassId $ Se.Eq purchasedPassId.getId,
          Se.Is Beam.startDate $ Se.Eq oldStartDate,
          Se.Is Beam.endDate $ Se.Eq oldEndDate
        ]
    ]

updateStatusAndProfilePictureByOrderId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  DPurchasedPass.StatusType ->
  Maybe Text ->
  Id DOrder.PaymentOrder ->
  m ()
updateStatusAndProfilePictureByOrderId status profilePicture orderId = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.status status, Se.Set Beam.profilePicture profilePicture, Se.Set Beam.updatedAt _now]
    [Se.Is Beam.orderId $ Se.Eq orderId.getId]

updatePurchasedPassIdByOldPurchasedPassId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DPurchasedPass.PurchasedPass ->
  Id DPurchasedPass.PurchasedPass ->
  m ()
updatePurchasedPassIdByOldPurchasedPassId newPurchasedPassId oldPurchasedPassId = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.purchasedPassId (getId newPurchasedPassId), Se.Set Beam.updatedAt _now]
    [Se.Is Beam.purchasedPassId $ Se.Eq (getId oldPurchasedPassId)]

updateStatusByPurchasedPassIdAndStatusAndStartDate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  DPurchasedPass.StatusType ->
  Id DPurchasedPass.PurchasedPass ->
  DPurchasedPass.StatusType ->
  Day ->
  m ()
updateStatusByPurchasedPassIdAndStatusAndStartDate newStatus purchasedPassId oldStatus startDate = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.status newStatus, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.purchasedPassId $ Se.Eq purchasedPassId.getId,
          Se.Is Beam.status $ Se.Eq oldStatus,
          Se.Is Beam.startDate $ Se.Eq startDate
        ]
    ]

-- | Mirror a spend into the DB, but only ever downwards.
--
-- Redis is the source of truth for the trip count; this column is a mirror that only matters if
-- the key is ever lost, at which point seededRemainingTrips reseeds from it. The Redis DECRBY is
-- atomic, but this write is not part of it, so two concurrent spends can decrement Redis to 10
-- then 9 and still land these writes in the reverse order, leaving the mirror at 10. A reseed
-- would then hand back a trip nobody paid for.
--
-- The GreaterThan predicate makes the write a no-op unless it lowers the stored value, so a
-- reordered pair converges on the smaller one. Refunds deliberately raise the count and must keep
-- using the plain setter.
--
-- The null arm is not optional: the column ships nullable with no backfill, and NULL > n is NULL
-- in SQL, so without it the very first spend on every pass would silently fail to mirror.
updateAvailableTripCountIfLowerById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Int ->
  Id DPurchasedPassPayment.PurchasedPassPayment ->
  m ()
updateAvailableTripCountIfLowerById availableTripCount id = do
  _now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.availableTripCount (Just availableTripCount), Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id.getId,
          Se.Or
            [ Se.Is Beam.availableTripCount $ Se.Eq Nothing,
              Se.Is Beam.availableTripCount $ Se.GreaterThan (Just availableTripCount)
            ]
        ]
    ]
