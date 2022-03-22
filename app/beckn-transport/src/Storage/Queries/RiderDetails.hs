module Storage.Queries.RiderDetails where

import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.RiderDetails
import Storage.Tabular.RiderDetails

create :: RiderDetails -> SqlDB ()
create = Esq.create'

findById ::
  Transactionable m =>
  Id RiderDetails ->
  m (Maybe RiderDetails)
findById = Esq.findById

findByMobileNumber ::
  (Transactionable m, EncFlow m r) =>
  Text ->
  m (Maybe RiderDetails)
findByMobileNumber mobileNumber_ = do
  mobileNumberDbHash <- getDbHash mobileNumber_
  Esq.findOne $ do
    riderDetails <- from $ table @RiderDetailsT
    where_ $ riderDetails ^. RiderDetailsMobileNumberHash ==. val mobileNumberDbHash
    return riderDetails
