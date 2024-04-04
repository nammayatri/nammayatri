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
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import Kernel.Utils.Error.Throwing
import qualified Sequelize as Se
import qualified Storage.Beam.Image as BeamI
import qualified Storage.CachedQueries.Merchant.TransporterConfig as QTC
import Storage.Queries.OrphanInstances.Image
import qualified Storage.Queries.Person as QP
import Tools.Error

-- Extra code goes here --
findRecentByPersonIdAndImageType :: KvDbFlow m r => Id Person -> DocumentType -> m [Image]
findRecentByPersonIdAndImageType personId imgtype = do
  person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  transporterConfig <- QTC.findByMerchantOpCityId person.merchantOperatingCityId (Just personId.getId) (Just "driverId") >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId)
  let onboardingRetryTimeInHours = transporterConfig.onboardingRetryTimeInHours
      onBoardingRetryTimeInHours' = intToNominalDiffTime onboardingRetryTimeInHours
  now <- getCurrentTime
  findAllWithKV
    [ Se.And
        [Se.Is BeamI.personId $ Se.Eq $ getId personId, Se.Is BeamI.imageType $ Se.Eq imgtype, Se.Is BeamI.createdAt $ Se.GreaterThanOrEq (hoursAgo onBoardingRetryTimeInHours' now)]
    ]
  where
    hoursAgo i now = negate (3600 * i) `DT.addUTCTime` now
