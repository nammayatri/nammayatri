{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.ImageExtra where

import qualified Data.Time as DT
import Domain.Types.DocumentVerificationConfig
import Domain.Types.Image
import Domain.Types.Merchant
import Domain.Types.Person (Person)
import Kernel.Beam.Functions
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Cac
import Kernel.Types.Common
import Kernel.Types.Documents
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import Kernel.Utils.Error.Throwing
import qualified Sequelize as Se
import qualified Storage.Beam.Image as BeamI
import qualified Storage.Cac.TransporterConfig as QTC
import Storage.Queries.OrphanInstances.Image
import qualified Storage.Queries.Person as QP
import Tools.Error
import Utils.Common.Cac.KeyNameConstants

-- Extra code goes here --
findRecentByPersonIdAndImageType :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> DocumentType -> m [Image]
findRecentByPersonIdAndImageType personId imgtype = do
  person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  transporterConfig <- QTC.findByMerchantOpCityId person.merchantOperatingCityId (Just (DriverId (cast personId))) >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId)
  let onboardingRetryTimeInHours = transporterConfig.onboardingRetryTimeInHours
      onBoardingRetryTimeInHours' = intToNominalDiffTime onboardingRetryTimeInHours
  now <- getCurrentTime
  findAllWithKV
    [ Se.And
        [Se.Is BeamI.personId $ Se.Eq $ getId personId, Se.Is BeamI.imageType $ Se.Eq imgtype, Se.Is BeamI.createdAt $ Se.GreaterThanOrEq (hoursAgo onBoardingRetryTimeInHours' now)]
    ]
  where
    hoursAgo i now = negate (3600 * i) `DT.addUTCTime` now

findImageByPersonIdAndImageTypeAndVerificationStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> DocumentType -> [VerificationStatus] -> m [Image]
findImageByPersonIdAndImageTypeAndVerificationStatus personId imgtype verificationStatus = do
  findAllWithKV
    [ Se.And
        [Se.Is BeamI.personId $ Se.Eq $ getId personId, Se.Is BeamI.imageType $ Se.Eq imgtype, Se.Is BeamI.verificationStatus $ Se.In (Just <$> verificationStatus)]
    ]

updateVerificationStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Documents.VerificationStatus -> Kernel.Types.Id.Id Domain.Types.Image.Image -> m ())
updateVerificationStatus verificationStatus (Kernel.Types.Id.Id id) = do
  _now <- getCurrentTime
  updateWithKV [Se.Set BeamI.verificationStatus (Just verificationStatus), Se.Set BeamI.updatedAt _now] [Se.Is BeamI.id $ Se.Eq id]

updateVerificationStatusByIdAndType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Documents.VerificationStatus -> Kernel.Types.Id.Id Domain.Types.Image.Image -> DocumentType -> m ())
updateVerificationStatusByIdAndType verificationStatus (Kernel.Types.Id.Id id) imageType = do
  updateOneWithKV
    [Se.Set BeamI.verificationStatus (Just verificationStatus)]
    [ Se.And
        [ Se.Is BeamI.id $ Se.Eq id,
          Se.Is BeamI.imageType $ Se.Eq imageType
        ]
    ]

updateVerificationStatusAndFailureReason ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Documents.VerificationStatus -> DriverOnboardingError -> Kernel.Types.Id.Id Domain.Types.Image.Image -> m ())
updateVerificationStatusAndFailureReason verificationStatus failureReason (Kernel.Types.Id.Id id) = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set BeamI.verificationStatus (Just verificationStatus), Se.Set BeamI.failureReason (Just failureReason), Se.Set BeamI.updatedAt _now] [Se.Is BeamI.id $ Se.Eq id]
