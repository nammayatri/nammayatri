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
import Kernel.Beam.Functions
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import Kernel.Utils.Error.Throwing
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOnboarding.Image as BeamI
import qualified Storage.CachedQueries.Merchant.MerchantConfig as QTC
import qualified Storage.Queries.Person as QP

create :: MonadFlow m => Image -> m ()
create = createWithKV

findById :: MonadFlow m => Id Image -> m (Maybe Image)
findById (Id imageid) = findOneWithKV [Se.Is BeamI.id $ Se.Eq imageid]

findImagesByPersonAndType :: MonadFlow m => Id Merchant -> Id Person -> ImageType -> m [Image]
findImagesByPersonAndType (Id merchantId) (Id personId) imgType =
  findAllWithKV
    [ Se.And
        [Se.Is BeamI.personId $ Se.Eq personId, Se.Is BeamI.merchantId $ Se.Eq merchantId, Se.Is BeamI.imageType $ Se.Eq imgType]
    ]

findRecentByPersonIdAndImageType :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> ImageType -> m [Image]
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

updateToValid :: MonadFlow m => Id Image -> m ()
updateToValid (Id id) =
  updateWithKV
    [Se.Set BeamI.isValid True]
    [Se.Is BeamI.id (Se.Eq id)]

findByMerchantId :: MonadFlow m => Id Merchant -> m [Image]
findByMerchantId (Id merchantId) = findAllWithKV [Se.Is BeamI.merchantId $ Se.Eq merchantId]

addFailureReason :: MonadFlow m => Id Image -> DriverOnboardingError -> m ()
addFailureReason (Id id) reason =
  updateWithKV
    [Se.Set BeamI.failureReason $ Just reason]
    [Se.Is BeamI.id (Se.Eq id)]

deleteByPersonId :: MonadFlow m => Id Person -> m ()
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
