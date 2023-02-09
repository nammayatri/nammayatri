module Storage.Queries.RiderDetails where

import Domain.Types.RiderDetails
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.Tabular.RiderDetails

create :: RiderDetails -> SqlDB ()
create = Esq.create

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
