module Storage.Queries.RiderDetailsExtra where

import Data.Maybe
import Domain.Types.DriverReferral
import Domain.Types.Merchant
import Domain.Types.Person
import Domain.Types.RiderDetails as DRDD
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.RiderDetails as BeamRD
import Storage.Queries.OrphanInstances.RiderDetails ()
import Tools.Error (RiderDetailsError (..))

-- Extra code goes here --

findByMobileNumberAndMerchant :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Text -> Id Merchant -> m (Maybe RiderDetails)
findByMobileNumberAndMerchant mobileNumber_ (Id merchantId) = do
  mobileNumberDbHash <- getDbHash mobileNumber_
  findOneWithKV [Se.And [Se.Is BeamRD.mobileNumberHash $ Se.Eq mobileNumberDbHash, Se.Is BeamRD.merchantId $ Se.Eq merchantId]]

findByMobileNumberAndMerchantAndBapId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Text -> Id Merchant -> Text -> m (Maybe RiderDetails)
findByMobileNumberAndMerchantAndBapId mobileNumber_ (Id merchantId) bapId_ = do
  mobileNumberDbHash <- getDbHash mobileNumber_
  findOneWithKV [Se.And [Se.Is BeamRD.mobileNumberHash $ Se.Eq mobileNumberDbHash, Se.Is BeamRD.merchantId $ Se.Eq merchantId, Se.Is BeamRD.bapId $ Se.Eq (Just bapId_)]]

findByMobileNumberHashAndMerchant :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DbHash -> Id Merchant -> m (Maybe RiderDetails)
findByMobileNumberHashAndMerchant mobileNumberDbHash (Id merchantId) = findOneWithKV [Se.And [Se.Is BeamRD.mobileNumberHash $ Se.Eq mobileNumberDbHash, Se.Is BeamRD.merchantId $ Se.Eq merchantId]]

updateReferralInfo :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DbHash -> Id Merchant -> Id DriverReferral -> Id Person -> m ()
updateReferralInfo customerNumberHash merchantId referralId driverId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamRD.referralCode (Just $ getId referralId),
      Se.Set BeamRD.referredByDriver (Just $ getId driverId),
      Se.Set BeamRD.referredAt (Just now)
    ]
    [Se.And [Se.Is BeamRD.mobileNumberHash (Se.Eq customerNumberHash), Se.Is BeamRD.merchantId (Se.Eq $ getId merchantId)]]

findAllRiderDetailsWithOptions :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> Int -> Int -> Maybe UTCTime -> Maybe UTCTime -> Bool -> Maybe (Id Person) -> Maybe DbHash -> m [RiderDetails]
findAllRiderDetailsWithOptions merchantId limit offset mbFrom mbTo areActivatedRidesOnly mbReferredByDriver mbMobileNumberHash = do
  findAllWithOptionsKV
    [ Se.And
        ( [Se.Is BeamRD.createdAt $ Se.GreaterThanOrEq (fromJust mbFrom) | isJust mbFrom]
            <> [Se.Is BeamRD.createdAt $ Se.LessThanOrEq (fromJust mbTo) | isJust mbTo]
            <> [Se.Is BeamRD.merchantId $ Se.Eq merchantId.getId]
            <> [Se.Is BeamRD.referredByDriver $ Se.Eq (mbReferredByDriver <&> (.getId)) | isJust mbReferredByDriver]
            <> [Se.Is BeamRD.referredByDriver $ Se.Not $ Se.Eq Nothing] -- filter the ones which are referred
            <> [Se.Is BeamRD.mobileNumberHash $ Se.Eq (fromJust mbMobileNumberHash) | isJust mbMobileNumberHash]
            <> [Se.Is BeamRD.payoutFlagReason $ Se.Eq Nothing | areActivatedRidesOnly == True] -- show only which are not frauds i.e. Nothing as flag
        )
    ]
    (Se.Desc BeamRD.createdAt)
    (Just limit)
    (Just offset)

updateCancelledRidesCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m ()
updateCancelledRidesCount riderId = do
  now <- getCurrentTime
  riderDetails <- findOneWithKV [Se.Is BeamRD.id (Se.Eq riderId)] >>= fromMaybeM (RiderDetailsNotFound riderId)
  updateOneWithKV
    [ Se.Set BeamRD.cancelledRides (Just (riderDetails.cancelledRides + 1)),
      Se.Set BeamRD.updatedAt now
    ]
    [Se.Is BeamRD.id (Se.Eq riderId)]

updateValidCancellationsCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m ()
updateValidCancellationsCount riderId = do
  now <- getCurrentTime
  riderDetails <- findOneWithKV [Se.Is BeamRD.id (Se.Eq riderId)] >>= fromMaybeM (RiderDetailsNotFound riderId)
  updateOneWithKV
    [ Se.Set BeamRD.validCancellations (Just (riderDetails.validCancellations + 1)),
      Se.Set BeamRD.updatedAt now
    ]
    [Se.Is BeamRD.id (Se.Eq riderId)]

updateCancellationDueRidesCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m ()
updateCancellationDueRidesCount riderId = do
  now <- getCurrentTime
  riderDetails <- findOneWithKV [Se.Is BeamRD.id (Se.Eq riderId)] >>= fromMaybeM (RiderDetailsNotFound riderId)
  updateOneWithKV
    [ Se.Set BeamRD.cancellationDueRides (Just (riderDetails.cancellationDueRides + 1)),
      Se.Set BeamRD.updatedAt now
    ]
    [Se.Is BeamRD.id (Se.Eq riderId)]

decrementCancellationDueRidesCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m ()
decrementCancellationDueRidesCount riderId = do
  now <- getCurrentTime
  riderDetails <- findOneWithKV [Se.Is BeamRD.id (Se.Eq riderId)] >>= fromMaybeM (RiderDetailsNotFound riderId)
  updateOneWithKV
    [ Se.Set BeamRD.cancellationDueRides (Just (max 0 (riderDetails.cancellationDueRides - 1))),
      Se.Set BeamRD.updatedAt now
    ]
    [Se.Is BeamRD.id (Se.Eq riderId)]

updateTotalBookingsCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m ()
updateTotalBookingsCount riderId = do
  now <- getCurrentTime
  riderDetails <- findOneWithKV [Se.Is BeamRD.id (Se.Eq riderId)] >>= fromMaybeM (RiderDetailsNotFound riderId)
  updateOneWithKV
    [ Se.Set BeamRD.totalBookings (Just (riderDetails.totalBookings + 1)),
      Se.Set BeamRD.updatedAt now
    ]
    [Se.Is BeamRD.id (Se.Eq riderId)]

updateCancellationDuesPaid :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => HighPrecMoney -> Text -> m ()
updateCancellationDuesPaid cancellationDuesPaid riderId = do
  now <- getCurrentTime
  riderDetails <- findOneWithKV [Se.Is BeamRD.id (Se.Eq riderId)] >>= fromMaybeM (RiderDetailsNotFound riderId)
  updateOneWithKV
    [ Se.Set BeamRD.cancellationDuesPaid (Just (riderDetails.cancellationDuesPaid + cancellationDuesPaid)),
      Se.Set BeamRD.updatedAt now
    ]
    [Se.Is BeamRD.id (Se.Eq riderId)]

updateNoOfTimesCanellationDuesPaid :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m ()
updateNoOfTimesCanellationDuesPaid riderId = do
  now <- getCurrentTime
  riderDetails <- findOneWithKV [Se.Is BeamRD.id (Se.Eq riderId)] >>= fromMaybeM (RiderDetailsNotFound riderId)
  updateOneWithKV
    [ Se.Set BeamRD.noOfTimesCanellationDuesPaid (Just (riderDetails.noOfTimesCanellationDuesPaid + 1)),
      Se.Set BeamRD.updatedAt now
    ]
    [Se.Is BeamRD.id (Se.Eq riderId)]

updateCancellationDuesPaymentInfo :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => HighPrecMoney -> Text -> m ()
updateCancellationDuesPaymentInfo amountPaid riderId = do
  now <- getCurrentTime
  riderDetails <- findOneWithKV [Se.Is BeamRD.id (Se.Eq riderId)] >>= fromMaybeM (RiderDetailsNotFound riderId)
  updateOneWithKV
    [ Se.Set BeamRD.cancellationDuesPaid (Just (riderDetails.cancellationDuesPaid + amountPaid)),
      Se.Set BeamRD.noOfTimesCanellationDuesPaid (Just (riderDetails.noOfTimesCanellationDuesPaid + 1)),
      Se.Set BeamRD.validCancellations (Just (riderDetails.validCancellations + 1)),
      Se.Set BeamRD.cancellationDueRides (Just (max 0 (riderDetails.cancellationDueRides - 1))),
      Se.Set BeamRD.updatedAt now
    ]
    [Se.Is BeamRD.id (Se.Eq riderId)]

updateNoOfTimesWaiveOffUsed :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m ()
updateNoOfTimesWaiveOffUsed riderId = do
  now <- getCurrentTime
  riderDetails <- findOneWithKV [Se.Is BeamRD.id (Se.Eq riderId)] >>= fromMaybeM (RiderDetailsNotFound riderId)
  updateOneWithKV
    [ Se.Set BeamRD.noOfTimesWaiveOffUsed (Just (riderDetails.noOfTimesWaiveOffUsed + 1)),
      Se.Set BeamRD.updatedAt now
    ]
    [Se.Is BeamRD.id (Se.Eq riderId)]

updateWaivedOffAmount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => HighPrecMoney -> Text -> m ()
updateWaivedOffAmount waivedOffAmount riderId = do
  now <- getCurrentTime
  riderDetails <- findOneWithKV [Se.Is BeamRD.id (Se.Eq riderId)] >>= fromMaybeM (RiderDetailsNotFound riderId)
  updateOneWithKV
    [ Se.Set BeamRD.waivedOffAmount (Just (riderDetails.waivedOffAmount + waivedOffAmount)),
      Se.Set BeamRD.updatedAt now
    ]
    [Se.Is BeamRD.id (Se.Eq riderId)]

updateCompletedRidesCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m ()
updateCompletedRidesCount riderId = do
  now <- getCurrentTime
  riderDetails <- findOneWithKV [Se.Is BeamRD.id (Se.Eq riderId)] >>= fromMaybeM (RiderDetailsNotFound riderId)
  updateOneWithKV
    [ Se.Set BeamRD.completedRides (Just (riderDetails.completedRides + 1)),
      Se.Set BeamRD.updatedAt now
    ]
    [Se.Is BeamRD.id (Se.Eq riderId)]
