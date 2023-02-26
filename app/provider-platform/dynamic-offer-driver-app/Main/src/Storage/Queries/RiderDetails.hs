module Storage.Queries.RiderDetails where

import Domain.Types.DriverReferral
import Domain.Types.Person
import Domain.Types.RiderDetails
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.Tabular.RiderDetails

create :: RiderDetails -> SqlDB ()
create = Esq.create

-- TODO :: write cached query for this
findById ::
  Transactionable m =>
  Id RiderDetails ->
  m (Maybe RiderDetails)
findById = Esq.findById

findByMobileNumber ::
  (MonadThrow m, Log m, Transactionable m, EncFlow m r) =>
  Text ->
  m (Maybe RiderDetails)
findByMobileNumber mobileNumber_ = do
  mobileNumberDbHash <- getDbHash mobileNumber_
  Esq.findOne $ do
    riderDetails <- from $ table @RiderDetailsT
    where_ $ riderDetails ^. RiderDetailsMobileNumberHash ==. val mobileNumberDbHash
    return riderDetails

updateHasTakenRide :: Id RiderDetails -> SqlDB ()
updateHasTakenRide riderId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RiderDetailsHasTakenRide =. val True,
        RiderDetailsUpdatedAt =. val now
      ]
    where_ $ tbl ^. RiderDetailsTId ==. val (toKey riderId)

findAllReferredByDriverId :: Transactionable m => Id Person -> m [RiderDetails]
findAllReferredByDriverId driverId = do
  Esq.findAll $ do
    riderDetails <- from $ table @RiderDetailsT
    where_ $ riderDetails ^. RiderDetailsReferredByDriver ==. val (Just $ toKey driverId)
    return riderDetails

updateReferralInfo ::
  DbHash ->
  Id DriverReferral ->
  Id Person ->
  SqlDB ()
updateReferralInfo customerNumberHash referralId driverId = do
  now <- getCurrentTime
  Esq.update $ \rd -> do
    set
      rd
      [ RiderDetailsReferralCode =. val (Just $ toKey referralId),
        RiderDetailsReferredByDriver =. val (Just $ toKey driverId),
        RiderDetailsReferredAt =. val (Just now)
      ]
    where_ $
      rd ^. RiderDetailsMobileNumberHash ==. val customerNumberHash
