{-# LANGUAGE TypeApplications #-}

module Storage.Queries.DriverReferral where

import Domain.Types.DriverReferral
import qualified Domain.Types.Person as SP
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.DriverReferral

create :: DriverReferral -> SqlDB ()
create = Esq.create

findByRefferalCode :: Transactionable m => Id DriverReferral -> m (Maybe DriverReferral)
findByRefferalCode = Esq.findById

findById ::
  Transactionable m =>
  Id SP.Person ->
  m (Maybe DriverReferral)
findById driverId = do
  findOne $ do
    driverReferral <- from $ table @DriverReferralT
    where_ $ driverReferral ^. DriverReferralDriverId ==. val (toKey driverId)
    return driverReferral

increaseTotalActivatedCustomerCount :: Id SP.Person -> Int -> SqlDB ()
increaseTotalActivatedCustomerCount driverId activatedCustomerCount = do
  Esq.update $ \tab -> do
    set
      tab
      [ DriverReferralActivatedCustomerCount =. val (activatedCustomerCount + 1)
      ]
    where_ $ tab ^. DriverReferralDriverId ==. val (toKey driverId)

increaseReferredCustomerCount :: Id SP.Person -> Int -> SqlDB ()
increaseReferredCustomerCount driverId referredCustomerCount = do
  Esq.update $ \tab -> do
    set
      tab
      [ DriverReferralReferredCustomerCount =. val (referredCustomerCount + 1)
      ]
    where_ $ tab ^. DriverReferralDriverId ==. val (toKey driverId)
