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

create :: IdfyVerification -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id IdfyVerification ->
  m (Maybe IdfyVerification)
findById = Esq.findById

findAllByDriverId ::
  Transactionable m =>
  Id Person ->
  m [IdfyVerification]
findAllByDriverId driverId = do
  findAll $ do
    verifications <- from $ table @IdfyVerificationT
    where_ $ verifications ^. IdfyVerificationDriverId ==. val (toKey driverId)
    return verifications

findLatestByDriverIdAndDocType ::
  Transactionable m =>
  Id Person ->
  ImageType ->
  m (Maybe IdfyVerification)
findLatestByDriverIdAndDocType driverId imgType = do
  verifications_ <- findAll $ do
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
  Transactionable m =>
  Text ->
  m (Maybe IdfyVerification)
findByRequestId requestId = do
  findOne $ do
    verification <- from $ table @IdfyVerificationT
    where_ $ verification ^. IdfyVerificationRequestId ==. val requestId
    return verification

updateResponse ::
  Text ->
  Text ->
  Text ->
  SqlDB ()
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

updateExtractValidationStatus :: Text -> ImageExtractionValidation -> SqlDB ()
updateExtractValidationStatus requestId status = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ IdfyVerificationImageExtractionValidation =. val status,
        IdfyVerificationUpdatedAt =. val now
      ]
    where_ $ tbl ^. IdfyVerificationRequestId ==. val requestId

deleteByPersonId :: Id Person -> SqlDB ()
deleteByPersonId personId =
  Esq.delete $ do
    verifications <- from $ table @IdfyVerificationT
    where_ $ verifications ^. IdfyVerificationDriverId ==. val (toKey personId)
