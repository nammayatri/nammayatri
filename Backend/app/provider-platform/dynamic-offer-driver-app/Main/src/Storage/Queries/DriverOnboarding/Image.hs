{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.DriverOnboarding.Image where

import qualified Data.Time as DT
import Domain.Types.DriverOnboarding.Error
import Domain.Types.DriverOnboarding.Image
import Domain.Types.Merchant
import Domain.Types.Person (Person)
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Error.Throwing
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOnboarding.Image as BeamI
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant.TransporterConfig as QTC
import qualified Storage.Queries.Person as QP

-- create :: Image -> SqlDB ()
-- create = Esq.create

create :: (L.MonadFlow m, Log m) => Image -> m ()
create = createWithKV

-- findById ::
--   Transactionable m =>
--   Id Image ->
--   m (Maybe Image)
-- findById = Esq.findById

findById :: (L.MonadFlow m, Log m) => Id Image -> m (Maybe Image)
findById (Id imageid) = findOneWithKV [Se.Is BeamI.id $ Se.Eq imageid]

-- findImagesByPersonAndType ::
--   (Transactionable m) =>
--   Id Merchant ->
--   Id Person ->
--   ImageType ->
--   m [Image]
-- findImagesByPersonAndType merchantId personId imgType = do
--   findAll $ do
--     images <- from $ table @ImageT
--       images ^. ImagePersonId ==. val (toKey personId)
--         &&. images ^. ImageImageType ==. val imgType
--         &&. images ^. ImageMerchantId ==. val (toKey merchantId)
--     return images

findImagesByPersonAndType :: (L.MonadFlow m, Log m) => Id Merchant -> Id Person -> ImageType -> m [Image]
findImagesByPersonAndType (Id merchantId) (Id personId) imgType =
  findAllWithKV
    [ Se.And
        [Se.Is BeamI.personId $ Se.Eq personId, Se.Is BeamI.merchantId $ Se.Eq merchantId, Se.Is BeamI.imageType $ Se.Eq imgType]
    ]

-- findRecentByPersonIdAndImageType ::
--   ( Transactionable m,
--     MonadFlow m,
--     CacheFlow m r,
--     EsqDBFlow m r,
--     EsqDBReplicaFlow m r
--   ) =>
--   Id Person ->
--   ImageType ->
--   m [Image]
-- findRecentByPersonIdAndImageType personId imgtype = do
--   -- person <- runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
--   person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
--   transporterConfig <- QTC.findByMerchantId person.merchantId >>= fromMaybeM (TransporterConfigNotFound person.merchantId.getId)
--   let onboardingRetryTimeInHours = transporterConfig.onboardingRetryTimeInHours
--   let onBoardingRetryTimeInHours = intToNominalDiffTime onboardingRetryTimeInHours
--   now <- getCurrentTime
--   findAll $ do
--     images <- from $ table @ImageT
--     where_ $
--       images ^. ImagePersonId ==. val (toKey personId)
--         &&. images ^. ImageImageType ==. val imgtype
--         &&. images ^. ImageCreatedAt >. val (hoursAgo onBoardingRetryTimeInHours now)
--     return images
--   where
--     hoursAgo i now = negate (3600 * i) `DT.addUTCTime` now

findRecentByPersonIdAndImageType :: (L.MonadFlow m, Log m, MonadTime m, CacheFlow m r, EsqDBFlow m r) => Id Person -> ImageType -> m [Image]
findRecentByPersonIdAndImageType personId imgtype = do
  person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  transporterConfig <- QTC.findByMerchantId person.merchantId >>= fromMaybeM (TransporterConfigNotFound person.merchantId.getId)
  let onboardingRetryTimeInHours = transporterConfig.onboardingRetryTimeInHours
      onBoardingRetryTimeInHours' = intToNominalDiffTime onboardingRetryTimeInHours
  now <- getCurrentTime
  findAllWithKV
    [ Se.And
        [Se.Is BeamI.personId $ Se.Eq $ getId personId, Se.Is BeamI.imageType $ Se.Eq imgtype, Se.Is BeamI.createdAt $ Se.GreaterThanOrEq (hoursAgo onBoardingRetryTimeInHours' now)]
    ]
  where
    hoursAgo i now = negate (3600 * i) `DT.addUTCTime` now

-- updateToValid :: Id Image -> SqlDB ()
-- updateToValid id = do
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ImageIsValid =. val True]

updateToValid :: (L.MonadFlow m, Log m) => Id Image -> m ()
updateToValid (Id id) =
  updateWithKV
    [Se.Set BeamI.isValid True]
    [Se.Is BeamI.id (Se.Eq id)]

-- findByMerchantId ::
--   Transactionable m =>
--   Id Merchant ->
--   m [Image]
-- findByMerchantId merchantId = do
--   findAll $ do
--     images <- from $ table @ImageT
--     where_ $ images ^. ImageMerchantId ==. val (toKey merchantId)
--     return images

findByMerchantId :: (L.MonadFlow m, Log m) => Id Merchant -> m [Image]
findByMerchantId (Id merchantId) = findAllWithKV [Se.Is BeamI.merchantId $ Se.Eq merchantId]

-- addFailureReason :: Id Image -> DriverOnboardingError -> SqlDB ()
-- addFailureReason id reason = do
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ImageFailureReason =. val (Just reason)]
--     where_ $ tbl ^. ImageTId ==. val (toKey id)

addFailureReason :: (L.MonadFlow m, Log m) => Id Image -> DriverOnboardingError -> m ()
addFailureReason (Id id) reason =
  updateWithKV
    [Se.Set BeamI.failureReason $ Just reason]
    [Se.Is BeamI.id (Se.Eq id)]

-- deleteByPersonId :: Id Person -> SqlDB ()
-- deleteByPersonId personId =
--   Esq.delete $ do
--     images <- from $ table @ImageT

deleteByPersonId :: (L.MonadFlow m, Log m) => Id Person -> m ()
deleteByPersonId (Id personId) = deleteWithKV [Se.Is BeamI.personId (Se.Eq personId)]

instance FromTType' BeamI.Image Image where
  fromTType' BeamI.ImageT {..} = do
    pure $
      Just
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

instance ToTType' BeamI.Image Image where
  toTType' Image {..} = do
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
