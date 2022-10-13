{-# LANGUAGE TypeApplications #-}

module Storage.Queries.DriverOnboarding.IdfyVerification where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.DriverOnboarding.IdfyVerification
import Domain.Types.DriverOnboarding.Image
import Domain.Types.Person (Person)
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

updateIdfyVerificationRec :: Text -> IdfyVerification -> SqlDB ()
updateIdfyVerificationRec requestId idfyVerification = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ IdfyVerificationImageExtractionValidation =. val idfyVerification.imageExtractionValidation,
        IdfyVerificationUpdatedAt =. val now
      ]
    where_ $ tbl ^. IdfyVerificationRequestId ==. val requestId
