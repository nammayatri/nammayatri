{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.DriverOnboarding.IdfyVerification where

import Domain.Types.DriverOnboarding.IdfyVerification
import Domain.Types.DriverOnboarding.Image
import Domain.Types.Person (Person)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.DriverOnboarding.IdfyVerification

create :: IdfyVerification -> SqlDB m ()
create = Esq.create

findById ::
  forall m ma.
  Transactionable ma m =>
  Proxy ma ->
  Id IdfyVerification ->
  m (Maybe IdfyVerification)
findById _ = Esq.findById @m @ma

findAllByDriverId ::
  forall m ma.
  Transactionable ma m =>
  Id Person ->
  Proxy ma ->
  m [IdfyVerification]
findAllByDriverId driverId _ = do
  findAll @m @ma $ do
    verifications <- from $ table @IdfyVerificationT
    where_ $ verifications ^. IdfyVerificationDriverId ==. val (toKey driverId)
    return verifications

findLatestByDriverIdAndDocType ::
  forall m ma.
  Transactionable ma m =>
  Id Person ->
  ImageType ->
  Proxy ma ->
  m (Maybe IdfyVerification)
findLatestByDriverIdAndDocType driverId imgType _ = do
  verifications_ <- findAll @m @ma $ do
    verifications <- from $ table @IdfyVerificationT
    where_ $
      verifications ^. IdfyVerificationDriverId ==. val (toKey driverId)
        &&. verifications ^. IdfyVerificationDocType ==. val imgType
    orderBy [desc $ verifications ^. IdfyVerificationCreatedAt]
    return verifications
  pure $ headMaybe verifications_
  where
    headMaybe [] = Nothing
    headMaybe (x : _) = Just x

findByRequestId ::
  forall m ma.
  Transactionable ma m =>
  Text ->
  Proxy ma ->
  m (Maybe IdfyVerification)
findByRequestId requestId _ = do
  findOne @m @ma $ do
    verification <- from $ table @IdfyVerificationT
    where_ $ verification ^. IdfyVerificationRequestId ==. val requestId
    return verification

updateResponse ::
  Text ->
  Text ->
  Text ->
  SqlDB m ()
updateResponse requestId status resp = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ IdfyVerificationStatus =. val status,
        IdfyVerificationIdfyResponse =. val (Just resp),
        IdfyVerificationUpdatedAt =. val now
      ]
    where_ $ tbl ^. IdfyVerificationRequestId ==. val requestId

updateExtractValidationStatus :: Text -> ImageExtractionValidation -> SqlDB m ()
updateExtractValidationStatus requestId status = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ IdfyVerificationImageExtractionValidation =. val status,
        IdfyVerificationUpdatedAt =. val now
      ]
    where_ $ tbl ^. IdfyVerificationRequestId ==. val requestId

deleteByPersonId :: Id Person -> SqlDB m ()
deleteByPersonId personId =
  Esq.delete $ do
    verifications <- from $ table @IdfyVerificationT
    where_ $ verifications ^. IdfyVerificationDriverId ==. val (toKey personId)
