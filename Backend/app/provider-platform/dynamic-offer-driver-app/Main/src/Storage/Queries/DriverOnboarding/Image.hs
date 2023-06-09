{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.DriverOnboarding.Image where

import qualified Data.Time as DT
import Domain.Types.DriverOnboarding.Error
import Domain.Types.DriverOnboarding.Image
import Domain.Types.Merchant
import Domain.Types.Person (Person)
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Error.Throwing
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOnboarding.Image as BeamI
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant.TransporterConfig as QTC
import qualified Storage.Queries.Person as QP
import Storage.Tabular.DriverOnboarding.Image

-- create :: Image -> SqlDB ()
-- create = Esq.create

create :: L.MonadFlow m => Image -> m (MeshResult ())
create image = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' Mesh.meshConfig (transformDomainImageToBeam image)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

-- findById ::
--   Transactionable m =>
--   Id Image ->
--   m (Maybe Image)
-- findById = Esq.findById

findById :: L.MonadFlow m => Id Image -> m (Maybe Image)
findById (Id imageid) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamImageToDomain <$>) <$> KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamI.id $ Se.Eq imageid]
    Nothing -> pure Nothing

-- findImagesByPersonAndType ::
--   (Transactionable m) =>
--   Id Merchant ->
--   Id Person ->
--   ImageType ->
--   m [Image]
-- findImagesByPersonAndType merchantId personId imgType = do
--   findAll $ do
--     images <- from $ table @ImageT
--     where_ $
--       images ^. ImagePersonId ==. val (toKey personId)
--         &&. images ^. ImageImageType ==. val imgType
--         &&. images ^. ImageMerchantId ==. val (toKey merchantId)
--     return images

findImagesByPersonAndType :: L.MonadFlow m => Id Merchant -> Id Person -> ImageType -> m [Image]
findImagesByPersonAndType (Id merchantId) (Id personId) imgType = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbCOnf' ->
      either (pure []) (transformBeamImageToDomain <$>)
        <$> KV.findAllWithKVConnector
          dbCOnf'
          Mesh.meshConfig
          [ Se.And
              [ Se.Is BeamI.personId $ Se.Eq personId,
                Se.Is BeamI.merchantId $ Se.Eq merchantId,
                Se.Is BeamI.imageType $ Se.Eq imgType
              ]
          ]
    Nothing -> pure []

findRecentByPersonIdAndImageType ::
  ( Transactionable m,
    MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r
  ) =>
  Id Person ->
  ImageType ->
  m [Image]
findRecentByPersonIdAndImageType personId imgtype = do
  -- person <- runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  transporterConfig <- QTC.findByMerchantId person.merchantId >>= fromMaybeM (TransporterConfigNotFound person.merchantId.getId)
  let onboardingRetryTimeInHours = transporterConfig.onboardingRetryTimeInHours
  let onBoardingRetryTimeInHours = intToNominalDiffTime onboardingRetryTimeInHours
  now <- getCurrentTime
  findAll $ do
    images <- from $ table @ImageT
    where_ $
      images ^. ImagePersonId ==. val (toKey personId)
        &&. images ^. ImageImageType ==. val imgtype
        &&. images ^. ImageCreatedAt >. val (hoursAgo onBoardingRetryTimeInHours now)
    return images
  where
    hoursAgo i now = negate (3600 * i) `DT.addUTCTime` now

findRecentByPersonIdAndImageType' :: (L.MonadFlow m, Log m, MonadTime m, CacheFlow m r, EsqDBFlow m r) => Id Person -> ImageType -> m [Image]
findRecentByPersonIdAndImageType' personId imgtype = do
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  transporterConfig <- QTC.findByMerchantId person.merchantId >>= fromMaybeM (TransporterConfigNotFound person.merchantId.getId)
  let onboardingRetryTimeInHours = transporterConfig.onboardingRetryTimeInHours
  let onBoardingRetryTimeInHours = intToNominalDiffTime onboardingRetryTimeInHours
  now <- getCurrentTime
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbCOnf' ->
      either (pure []) (transformBeamImageToDomain <$>)
        <$> KV.findAllWithKVConnector
          dbCOnf'
          Mesh.meshConfig
          [ Se.And
              [ Se.Is BeamI.personId $ Se.Eq $ getId personId,
                Se.Is BeamI.imageType $ Se.Eq imgtype,
                Se.Is BeamI.createdAt $ Se.GreaterThanOrEq (hoursAgo onBoardingRetryTimeInHours now)
              ]
          ]
    Nothing -> pure []
  where
    hoursAgo i now = negate (3600 * i) `DT.addUTCTime` now

-- updateToValid :: Id Image -> SqlDB ()
-- updateToValid id = do
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ImageIsValid =. val True]
--     where_ $ tbl ^. ImageTId ==. val (toKey id)

updateToValid :: L.MonadFlow m => Id Image -> m (MeshResult ())
updateToValid (Id id) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        [ Se.Set BeamI.isValid True
        ]
        [Se.Is BeamI.id (Se.Eq id)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

-- findByMerchantId ::
--   Transactionable m =>
--   Id Merchant ->
--   m [Image]
-- findByMerchantId merchantId = do
--   findAll $ do
--     images <- from $ table @ImageT
--     where_ $ images ^. ImageMerchantId ==. val (toKey merchantId)
--     return images

findByMerchantId :: L.MonadFlow m => Id Merchant -> m [Image]
findByMerchantId (Id merchantId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbCOnf' ->
      either (pure []) (transformBeamImageToDomain <$>)
        <$> KV.findAllWithKVConnector
          dbCOnf'
          Mesh.meshConfig
          [ Se.Is BeamI.merchantId $ Se.Eq merchantId
          ]
    Nothing -> pure []

-- addFailureReason :: Id Image -> DriverOnboardingError -> SqlDB ()
-- addFailureReason id reason = do
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ImageFailureReason =. val (Just reason)]
--     where_ $ tbl ^. ImageTId ==. val (toKey id)

addFailureReason :: L.MonadFlow m => Id Image -> DriverOnboardingError -> m (MeshResult ())
addFailureReason (Id id) reason = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        [ Se.Set BeamI.failureReason $ Just reason
        ]
        [Se.Is BeamI.id (Se.Eq id)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

-- deleteByPersonId :: Id Person -> SqlDB ()
-- deleteByPersonId personId =
--   Esq.delete $ do
--     images <- from $ table @ImageT
--     where_ $ images ^. ImagePersonId ==. val (toKey personId)

deleteByPersonId :: L.MonadFlow m => Id Person -> m ()
deleteByPersonId (Id personId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' ->
      void $
        KV.deleteWithKVConnector
          dbConf'
          Mesh.meshConfig
          [Se.Is BeamI.personId (Se.Eq personId)]
    Nothing -> pure ()

transformBeamImageToDomain :: BeamI.Image -> Image
transformBeamImageToDomain BeamI.ImageT {..} = do
  Image
    { id = Id id,
      personId = Id personId,
      merchantId = Id merchantId,
      s3Path = s3Path,
      imageType = imageType,
      isValid = isValid,
      failureReason = failureReason,
      createdAt = createdAt
    }

transformDomainImageToBeam :: Image -> BeamI.Image
transformDomainImageToBeam Image {..} =
  BeamI.ImageT
    { BeamI.id = getId id,
      BeamI.personId = getId personId,
      BeamI.merchantId = getId merchantId,
      BeamI.s3Path = s3Path,
      BeamI.imageType = imageType,
      BeamI.isValid = isValid,
      BeamI.failureReason = failureReason,
      BeamI.createdAt = createdAt
    }
