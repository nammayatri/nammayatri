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
