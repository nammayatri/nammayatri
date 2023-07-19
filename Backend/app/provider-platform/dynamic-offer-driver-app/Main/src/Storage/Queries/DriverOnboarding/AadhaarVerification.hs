{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.DriverOnboarding.AadhaarVerification where

import Domain.Types.DriverOnboarding.AadhaarVerification
import Domain.Types.Person (Person)
import Kernel.External.Encryption (DbHash)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.DriverOnboarding.AadhaarVerification

create :: AadhaarVerification -> Esq.SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id AadhaarVerification ->
  m (Maybe AadhaarVerification)
findById = Esq.findById

findByDriverId ::
  Transactionable m =>
  Id Person ->
  m (Maybe AadhaarVerification)
findByDriverId driverId = do
  findOne $ do
    aadhaar <- from $ table @AadhaarVerificationT
    where_ $ aadhaar ^. AadhaarVerificationDriverId ==. val (toKey driverId)
    return aadhaar

findByAadhaarNumberHash ::
  Transactionable m =>
  DbHash ->
  m (Maybe AadhaarVerification)
findByAadhaarNumberHash aadhaarHash = do
  findOne $ do
    aadhaar <- from $ table @AadhaarVerificationT
    where_ $ aadhaar ^. AadhaarVerificationAadhaarNumberHash ==. val (Just aadhaarHash)
    return aadhaar

deleteByDriverId :: Id Person -> SqlDB ()
deleteByDriverId driverId =
  Esq.delete $ do
    aadhaar <- from $ table @AadhaarVerificationT
    where_ $ aadhaar ^. AadhaarVerificationDriverId ==. val (toKey driverId)

findByPhoneNumberAndUpdate :: Text -> Text -> Text -> Maybe DbHash -> Bool -> Id Person -> Esq.SqlDB ()
findByPhoneNumberAndUpdate name gender dob aadhaarNumberHash isVerified personId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ AadhaarVerificationDriverName =. val name,
        AadhaarVerificationDriverGender =. val gender,
        AadhaarVerificationDriverDob =. val dob,
        AadhaarVerificationAadhaarNumberHash =. val aadhaarNumberHash,
        AadhaarVerificationIsVerified =. val isVerified,
        AadhaarVerificationUpdatedAt =. val now
      ]
    where_ $ tbl ^. AadhaarVerificationDriverId ==. val (toKey personId)

deleteByPersonId :: Id Person -> SqlDB ()
deleteByPersonId personId =
  Esq.delete $ do
    verifications <- from $ table @AadhaarVerificationT
    where_ $ verifications ^. AadhaarVerificationDriverId ==. val (toKey personId)
